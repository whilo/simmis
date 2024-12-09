(ns is.simm.views.main
  (:require  [is.simm.views.tasks :refer [TaskManager]]
             [is.simm.simulations.screenshare.view :refer [ScreenShare]]
             #?(:clj [is.simm.users :refer [get-session add-session! update-session! delete-session! get-user]])
             [hyperfiddle.electric :as e]
             [hyperfiddle.electric-dom2 :as dom]
             #?(:clj [datahike.api :as d])
             [missionary.core :as m]))

#?(:cljs
   (def !view-state (atom :main)))

#?(:cljs (def !rotation (atom 0)))

(e/defn Header [session-id]
  (e/client
   (let [user (e/server (get-user session-id))]
     (dom/header (dom/props {:class "bg-gray-600 p-4"})
                 (dom/div (dom/props {:class "container mx-auto flex justify-between items-center"})
                          (dom/div (dom/props {:class "flex items-center"})
                                   (dom/a (dom/props {:class "text-white text-xl font-bold"})
                                          (dom/on "click" (e/fn [_e] (reset! !view-state :main)))
                                          (dom/img (dom/props {:src "/simmis.png" :alt "Logo" :class "h-10" :style {:transform (str "rotate(" @!rotation "deg)")}})))
                                   (dom/text "immis"))
                          (dom/nav (dom/props {:class "flex space-x-4"})
                                   (dom/a (dom/props {:class "text-white" :href "#about"})
                                          (dom/on "click" (e/fn [_e] (reset! !view-state :about)))
                                          (dom/text "About"))
                                   #_(dom/a (dom/props {:class "text-white" :href "#taskmanager"})
                                            (dom/on "click" (e/fn [_e] (reset! !view-state :taskmanager)))
                                            (dom/text "Task Manager"))
                                   (dom/a (dom/props {:class "text-white" :href "#screenshare"})
                                          (dom/on "click" (e/fn [_e] (reset! !view-state :screenshare)))
                                          (dom/text "Screen Share"))
                                   (dom/a (dom/props {:class "text-white" :href "#user"})
                                          (dom/on "click" (e/fn [_e] (js/alert "not implemented")))
                                          (dom/text user))))))))


(e/defn Footer []
  (e/client
   (dom/footer (dom/props {:class "bg-gray-200 p-4"})
               (dom/div (dom/props {:class "container mx-auto flex justify-center text-center"})
                        (dom/p (dom/text "Copyright Christian Weilbach 2024. Powered by ")
                               (dom/a (dom/props {:href "https://clojure.org/" :target "_blank" :class "text-blue-400"})
                                      (dom/text "Clojure"))
                               (dom/text ", ")
                               (dom/a (dom/props {:href "https://datahike.io" :target "_blank" :class "text-blue-400"})
                                      (dom/text "Datahike"))
                               (dom/text " and ")
                               (dom/a (dom/props {:href "https://hyperfiddle.net/" :target "_blank" :class "text-blue-400"})
                                      (dom/text "Electric"))
                               (dom/text "."))))))




(e/defn LoginOrSignup []
  (e/client
   (let [submit-fn (e/fn [e]
                     (.preventDefault e)
                     (let [user (.-value (.getElementById js/document "user"))
                           password (.-value (.getElementById js/document "password"))]
                       #_(if (CheckLogin?. user password)
                           (do
                             (reset! !user {:user user :password password})
                             (reset! !view-state :main))
                           (do
                             (reset! !user nil)
                             (js/alert "Invalid username or password.")))))]
     (dom/div (dom/props {:class "flex flex-col items-center justify-center"})
              (dom/a (dom/props {:class "bg-blue-500 text-white font-bold rounded"
                                 ::dom/href "/auth"}) (dom/text "Login")))
     #_(dom/div (dom/props {:class "flex flex-col items-center justify-center"})
              (dom/div (dom/props {:class "w-full max-w-md"})
                       (dom/h1 (dom/props {:class "text-3xl font-bold mb-6"}) (dom/text "Signup"))
                       (dom/form (dom/props {:class "bg-white shadow-md rounded px-8 pt-6 pb-8 mb-4"})))
              (dom/on "submit" submit-fn)
              (dom/div (dom/props {:class "mb-4"})
                       (dom/label (dom/props {:class "block text-gray-700 text-sm font-bold mb-2"} (dom/text "Username")))
                       (dom/input (dom/props {:id "user" :class "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline" :type "text" :placeholder "Username"})))
              (dom/div (dom/props {:class "mb-6"})
                       (dom/label (dom/props {:class "block text-gray-700 text-sm font-bold mb-2"} (dom/text "Password")))
                       (dom/input (dom/props {:id "password" :class "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 mb-3 leading-tight focus:outline-none focus:shadow-outline" :type "password" :placeholder "Password"})))
              (dom/div (dom/props {:class "flex items-center justify-between"})
                       (dom/button (dom/props {:class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline" :type "submit"})
                                   (dom/on "click" submit-fn)
                                   (dom/text "Login")))))))


(e/defn About []
  (e/client
   (dom/section (dom/props {:class "flex items-center justify-center p-6"})
                (dom/div (dom/props {:class "max-w-4xl w-full p-8 bg-gray-100 rounded-lg text-center space-y-8"})
                         
                         ;; Title - centered
                         (dom/h1 (dom/props {:class "text-5xl font-extrabold text-gray-800"}) (dom/text "About"))

                         ;; Mission Section
                         (dom/h2 (dom/props {:class "text-3xl font-semibold text-gray-800"}) (dom/text "Our Mission"))
                         (dom/p (dom/props {:class "text-lg text-gray-700 text-left"}) 
                                (dom/text "Our mission is to create simple and effective task management solutions that help users stay organized and productive."))

                         ;; Features Section
                         (dom/h3 (dom/props {:class "text-2xl font-semibold text-gray-800"}) (dom/text "Features"))
                         (dom/p (dom/props {:class "text-lg text-gray-700 text-left"}) 
                                (dom/text "This application comes with a variety of features designed to improve your workflow, such as task categorization, priority levels, and deadlines."))

                         ;; Key Features List - centered
                         (dom/h4 (dom/props {:class "text-xl font-semibold text-gray-800"}) (dom/text "Key Features"))
                         (dom/ul (dom/props {:class "list-disc pl-6 space-y-2 text-left"})
                                 (dom/li (dom/props {:class "text-lg text-gray-700"}) (dom/text "Task prioritization"))
                                 (dom/li (dom/props {:class "text-lg text-gray-700"}) (dom/text "Deadline management"))
                                 (dom/li (dom/props {:class "text-lg text-gray-700"}) (dom/text "Task categorization"))
                                 (dom/li (dom/props {:class "text-lg text-gray-700"}) (dom/text "Collaborative task sharing"))
                                 (dom/li (dom/props {:class "text-lg text-gray-700"}) (dom/text "Progress tracking")))

                         ;; Additional Information
                         (dom/p (dom/props {:class "text-lg text-gray-700 text-left"}) 
                                (dom/text "With these features, our application provides an intuitive and powerful tool to help manage both simple and complex tasks."))

                         ;; Learn More Section
                         (dom/h3 (dom/props {:class "text-2xl font-semibold text-gray-800"}) (dom/text "Learn More"))
                         (dom/p (dom/props {:class "text-lg text-gray-700 text-left"}) 
                                (dom/text "You can learn more about our mission and features by visiting the following links:"))

                         ;; List of Links - centered
                         (dom/ul (dom/props {:class "list-inside space-y-2 text-left"})
                                 (dom/li (dom/props {:class "text-lg text-blue-600"}) (dom/a (dom/props {:href "https://www.example.com/mission" :class "hover:underline"}) (dom/text "Our Mission")))
                                 (dom/li (dom/props {:class "text-lg text-blue-600"}) (dom/a (dom/props {:href "https://www.example.com/features" :class "hover:underline"}) (dom/text "Our Features")))
                                 (dom/li (dom/props {:class "text-lg text-blue-600"}) (dom/a (dom/props {:href "https://www.example.com/contact" :class "hover:underline"}) (dom/text "Contact Us"))))

                         ;; Final Paragraph
                         (dom/p (dom/props {:class "text-lg text-gray-700 text-left"}) 
                                (dom/text "Feel free to reach out if you have any questions or need support. We're always happy to help!"))))))


(e/defn Main [ring-request]
  (let [view-state (e/client (e/watch !view-state))
        ;session-id (e/server (get-in ring-request [:headers "sec-websocket-key"]))
        session (e/server (get-session ring-request))]
    (prn "session" session)
    (e/client
     (binding [dom/node js/document.body]
       (dom/section (dom/props {:class "flex flex-col min-h-screen"})
                    (Header. session)
                    (if-not session
                      (LoginOrSignup.)
                      (dom/div (dom/props {:class "flex-grow"})
                               (case view-state
                                 :main (ScreenShare. session)
                                 :screenshare (ScreenShare. session)
                                 ;:taskmanager (TaskManager.)
                                 ;:login (Login.)
                                 :about (About.))))
                    (Footer.))))))