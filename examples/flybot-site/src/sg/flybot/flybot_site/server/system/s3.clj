(ns sg.flybot.flybot-site.server.system.s3
  "S3 operations for image uploads.

   Uses cognitect aws-api for streaming uploads to S3.

   Configuration:
   - S3_UPLOADS_BUCKET: Target bucket name
   - S3_UPLOADS_REGION: AWS region (default: ap-southeast-1)

   The module falls back to local filesystem if S3 is not configured,
   enabling local development without AWS credentials."
  (:require
   [cognitect.aws.client.api :as aws]
   [com.brunobonacci.mulog :as mu]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import [java.util UUID]))

(defonce ^:private s3-client (atom nil))

(defn- get-client
  "Get or create S3 client for the configured region."
  [region]
  (or @s3-client
      (reset! s3-client (aws/client {:api :s3 :region region}))))

(defn upload-to-s3!
  "Upload file to S3 bucket.

   Args:
   - bucket: S3 bucket name
   - region: AWS region
   - file: File or InputStream to upload
   - filename: Original filename (used for extension)

   Returns: {:url \"https://bucket.s3.region.amazonaws.com/key\"}"
  [bucket region file filename]
  (let [ext (-> filename (str/split #"\.") last)
        key (str "uploads/" (UUID/randomUUID) "." ext)
        client (get-client region)
        body (if (instance? java.io.File file)
               (io/input-stream file)
               file)]
    (mu/log ::s3-upload-start :bucket bucket :key key)
    (let [result (aws/invoke client {:op :PutObject
                                     :request {:Bucket bucket
                                               :Key key
                                               :Body body
                                               :ContentType (case ext
                                                              ("jpg" "jpeg") "image/jpeg"
                                                              "png" "image/png"
                                                              "gif" "image/gif"
                                                              "webp" "image/webp"
                                                              "application/octet-stream")}})]
      (if (:cognitect.anomalies/category result)
        (do
          (mu/log ::s3-upload-error :error result)
          (throw (ex-info "S3 upload failed" {:error result})))
        (let [url (str "https://" bucket ".s3." region ".amazonaws.com/" key)]
          (mu/log ::s3-upload-success :url url)
          {:url url})))))

(defn upload-local!
  "Upload file to local filesystem (fallback for dev).

   Args:
   - uploads-dir: Local directory path
   - file: File to copy
   - filename: Original filename

   Returns: {:url \"/uploads/filename\"}"
  [uploads-dir file filename]
  (let [dir (io/file uploads-dir)
        _ (when-not (.exists dir) (.mkdirs dir))
        ext (-> filename (str/split #"\.") last)
        new-filename (str (UUID/randomUUID) "." ext)
        dest-file (io/file uploads-dir new-filename)]
    (io/copy file dest-file)
    (mu/log ::local-upload-success :filename new-filename)
    {:url (str "/uploads/" new-filename)}))

(defn make-upload-handler
  "Create upload handler based on configuration.

   Config is typed with :type dispatch:
   - {:type :s3 :bucket \"...\" :region \"...\"}
   - {:type :local :dir \"...\"}

   Returns: (fn [tempfile filename] -> {:url ...})"
  [{:keys [type bucket region dir]}]
  (case type
    :s3
    (do
      (mu/log ::upload-handler-init :type :s3 :bucket bucket :region region)
      (fn [tempfile filename]
        (upload-to-s3! bucket region tempfile filename)))

    :local
    (do
      (mu/log ::upload-handler-init :type :local :dir dir)
      (fn [tempfile filename]
        (upload-local! dir tempfile filename)))))

^:rct/test
(comment
  ;; Local upload test
  (let [handler (make-upload-handler {:type :local :dir "/tmp/test-uploads"})
        tempfile (java.io.File/createTempFile "test" ".png")]
    (spit tempfile "fake image data")
    (:url (handler tempfile "image.png")))
  ;=>> #"/uploads/.*\.png"
  )
