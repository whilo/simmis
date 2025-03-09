(ns is.simm.users
  (:require [datahike.api :as d]))

(def user-cfg {:store {:backend :mem :id "user-manager"}})

(def user-schema [{:db/ident :user
                   :db/unique :db.unique/identity
                   :db/valueType :db.type/string
                   :db/cardinality :db.cardinality/one}
                  {:db/ident :email
                   :db/unique :db.unique/identity
                   :db/valueType :db.type/string
                   :db/cardinality :db.cardinality/one}
                  {:db/ident :password
                   :db/valueType :db.type/string
                   :db/cardinality :db.cardinality/one}
                  {:db/ident :role
                   :db/valueType :db.type/string
                   :db/cardinality :db.cardinality/one}])


#?(:clj
   (defn add-user! [conn user email password role]
     (d/transact conn [{:user user
                        :email email
                        :password password
                        :role role}])))

(def session-cfg {:store {:backend :mem :id "session-manager"}})

(def session-schema [{:db/ident :session-id
                      :db/unique :db.unique/identity
                      :db/valueType :db.type/string
                      :db/cardinality :db.cardinality/one}
                     {:db/ident :user
                      :db/valueType :db.type/string
                      :db/cardinality :db.cardinality/one}
                     {:db/ident :initiated
                      :db/valueType :db.type/instant
                      :db/cardinality :db.cardinality/one}
                     {:db/ident :last-access
                      :db/valueType :db.type/instant
                      :db/cardinality :db.cardinality/one}])

#?(:clj
   (defn init-db [cfg schema]
     (try
       (d/create-database cfg)
       (let [conn (d/connect cfg)]
         (d/transact conn schema))
       (catch Exception e
         (println "Database already exists")))))

#?(:clj
   (do
     (init-db user-cfg user-schema)
     (init-db session-cfg session-schema)))

#?(:clj (defonce session-conn (d/connect session-cfg)))

#?(:clj (defonce user-conn (d/connect user-cfg)))


#?(:clj
   (defn add-session! [conn session-id user]
     (d/transact conn [{:session-id session-id
                        :user user
                        :initiated (java.util.Date.)
                        :last-access (java.util.Date.)}])))

#?(:clj
   (defn update-session! [conn session-id]
     (d/transact conn [{:session-id session-id
                        :last-access (java.util.Date.)}])))

#?(:clj
   (defn get-session [ring-request]
     (let [session-id (get-in ring-request [:cookies "session" :value])
           session (d/q '[:find ?e .
                          :in $ ?session-id
                          :where
                          [?e :session-id ?session-id]]
                        @session-conn session-id)]
       (when session session-id))))

#?(:clj
   (defn get-user [session-id]
     (d/q '[:find ?user .
            :in $ ?session-id
            :where
            [?e :session-id ?session-id]
            [?e :user ?user]]
          @session-conn session-id)))

#?(:clj
   (defn delete-session! [conn session-id]
     (d/transact conn [[:db.fn/retractEntity session-id]])))



(comment
  ;; active sessions
  (d/q '[:find ?user .
         :in $ ?session-id
         :where
         [?e :session-id ?session-id]
         [?e :user ?user]]
       @session-conn "5c5a3c93-8d2e-4c1d-85ab-c786cc40e546")


  (d/q '[:find ?user ?session-id
         :in $
         :where
         [?e :session-id ?session-id]
         [?e :user ?user]]
       @session-conn)

  @user-conn

  ;; add a demo user
  (add-user! user-conn "tester" "tester@mail.com" "123" "admin"))



#?(:clj
   (add-user! user-conn "tester" "tester@mail.com" "123" "admin"))


#?(:clj (defn authenticate [username password]
          (let [user (d/q '[:find (pull ?e [:*]) .
                            :in $ ?username ?password
                            :where
                            [?e :user ?username]
                            [?e :password ?password]]
                          @user-conn username password)
                _ (prn "user" user)
                session-id (when-not (empty?  user) (str (java.util.UUID/randomUUID)))]
            (when session-id
              (add-session! session-conn session-id username))
            session-id))) 


(comment

  (d/q '[:find (pull ?e [:*]) .
         :in $
         :where
         [?e :user ?username]
         [?e :password ?password]]
       @user-conn))