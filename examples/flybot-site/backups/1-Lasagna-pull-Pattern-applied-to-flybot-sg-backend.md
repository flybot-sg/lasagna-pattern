---
title: Lasagna-pull Pattern applied to flybot.sg backend
id: 1
author: loic@basecity.com
tags:
  - Blog
created-at: "2023-08-05T00:00:00.000Z"
updated-at: "2023-08-07T00:00:00.000Z"
---

[lasagna-pull](https://github.com/flybot-sg/lasagna-pull) is an open-source library which allows the user to precisely select data in nested data structure.

We use it intensively in the website you are currently visiting in both backend and frontend.

In this article, I will focus on how we use in the backend.

## Defines API as pure data

A good use case of the pattern is as parameter in a post request.

In our backend, we have a structure representing all our endpoints:

```clojure
;; BACKEND data structure
(defn pullable-data
  "Path to be pulled with the pull-pattern.
   The pull-pattern `:with` option will provide the params to execute the function before pulling it."
  [db session]
  {:posts {:all (fn [] (get-all-posts db))
           :post (fn [post-id] (get-post db post-id))
           :new-post (with-role session :editor (fn [post] (add-post db post)))
           :removed-post (with-role session :editor (fn [post-id user-id] (delete-post db post-id user-id)))}
   :users {:all (with-role session :owner (fn [] (get-all-users db)))
           :user (fn [id] (get-user db id))
           :removed-user (with-role session :owner (fn [id] (delete-user db id)))
           :auth {:registered (fn [id email name picture] (register-user db id email name picture))
                  :logged (fn [] (login-user db (:user-id session)))}
           :new-role {:admin (with-role session :owner (fn [email] (grant-admin-role db email)))
                      :owner (with-role session :owner (fn [email] (grant-owner-role db email)))}
           :revoked-role {:admin (with-role session :owner (fn [email] (revoke-admin-role db email)))}}})
```

This resembles a REST API structure.

Since the API "route" information is contained within the pattern keys themselves, all the http requests with a pattern as params can hit the same backend URI.

So we have a single route for all pattern http request:

```clojure
(into (auth/auth-routes oauth2-config)
      [["/pattern" {:post ring-handler}] ;; all requests with pull pattern go here
       ["/users/logout" {:get (auth/logout-handler client-root-path)}]
       ["/oauth/google/success" {:get ring-handler :middleware [[auth/authentification-middleware client-root-path]]}]
       ["/*" {:get {:handler index-handler}}]])
```

Therefore the pull pattern:
- describes the API routes
- provides the data expected by the server in its :with option for the concerned endpoints
- describes what is asked by the client to only return relevant data
- can easily perform authorisation

## Example: pull a post

For instance, getting a specific post, meaning with the "route": :posts :post, can be done this way:

```clojure
((pull/qfn {:posts {(list :post :with [s/post-1-id]) ;; provide required params to pullable-data :post function
                    {:post/id '?
                     :post/page '?
                     :post/css-class '?
                     :post/creation-date '?
                     :post/last-edit-date '?
                     :post/author {:user/id '?
                                   :user/email '?
                                   :user/name '?
                                   :user/picture '?
                                   :user/roles [{:role/name '?
                                                 :role/date-granted '?}]}
                     :post/last-editor {:user/id '?
                                        :user/email '?
                                        :user/name '?
                                        :user/picture '?
                                        :user/roles [{:role/name '?
                                                      :role/date-granted '?}]}
                     :post/md-content '?
                     :post/image-beside {:image/src '?
                                         :image/src-dark '?
                                         :image/alt '?}
                     :post/default-order '?}}}
           '&? ;; bind the whole data
           ))
```

It is important to understand that the param s/post-1-id in `(list :post :with [#uuid s/post-1-id])` was passed to `(fn [post-id] (get-post db post-id))` in pullable-data. The function returned the post fetched from the db.

We decided to fetch all the information of the post in our pattern but we could have just fetch some of the keys only:

```clojure
((pull/qfn {:posts {(list :post :with [s/post-1-id]) ;; only fetch id and page even though all the other keys have been returned here
                    {:post/id '?
                     :post/page '?}}}
           '&?))
=> {:posts {:post {:post/id #uuid "64cda032-b4e4-431e-bd85-0dbe34a8feeb"
                   :post/page :home}}}
```

The function `(fn [post-id] (get-post db post-id))` returned all the post keys but we only select the post/id and post/page.

## Post data validation

It is common to use Malli schema to validate data.

Here is the malli schema for the post data structure we used above:

```clojure
(def post-schema
  [:map {:closed true}
   [:post/id :uuid]
   [:post/page :keyword]
   [:post/css-class {:optional true} [:string {:min 3}]]
   [:post/creation-date inst?]
   [:post/last-edit-date {:optional true} inst?]
   [:post/author user-schema]
   [:post/last-editor {:optional true} user-schema]
   [:post/md-content [:and [:string {:min 10}]
                      [:fn {:error/message "Level 1 Heading `#` missing in markdown."}
                       md/has-valid-h1-title?]]]
   [:post/image-beside {:optional true}
    [:map
     [:image/src [:string {:min 10}]]
     [:image/src-dark [:string {:min 10}]]
     [:image/alt [:string {:min 5}]]]]
   [:post/default-order {:optional true} nat-int?]])
```

## Pattern data validation

lasagna-pull also allows us to provide schema alongside the pattern to validate 2 things:
- the pattern format is correct
- the pattern content respects a malli schema

This is very good because we can have a malli schema for the entire pullable-data structure.

## Pattern query context

Earlier, I asked you to assume that the function from pullable-data was returning a post data structure.

In reality, it is a bit more complex than this because what is returned by the different functions in pullable-data is a map. For instance:

```clojure
;; returned by get-post
{:response (db/get-post db post-id)} ;; note the response key here

;; returned by register-user
{:response user
 :effects {:db {:payload [user]}} ;; the db transaction description to be made
 :session {:user-id user-id} ;; the user info to be added to the session
 }
```

This allows us to deal with pure data until the very last moment when we run all the side effects (db transaction and session) in one place only we call executor.
