(ns social-kata.core-test
  (:require [clojure.test :refer :all]
            [social-kata.core :refer :all]
            [clj-time.core :as t]))

(deftest publish-test
  (testing "Alice can publish to a personal timeline"
    (is (= {"alice" [{:message "hello world." :mentions #{}}]}
           (publish {} "alice" "hello world.")))))

(deftest publish-rec-test
  (testing "Alice can publish to a personal timeline"
    (let [time-before (t/now)
          username "alice"
          msg "hello world."
          state (publish-rec (new-world []) username msg)]
      (is (= (list username) (keys state)))
      (is (= username (get-in state [username :username])))
      (is (= username (get-in state [username :timeline 0 :author])))
      (is (= msg (get-in state [username :timeline 0 :message])))
      (let [timestamp
            (get-in state [username :timeline 0 :timestamp])]
        (is (t/within? (t/interval time-before
                                   (t/plus time-before (t/seconds 1)))
                       timestamp)
            (format ":timestamp %s is not within a second of %s" timestamp time-before))))))

(deftest reading-test
  (testing "bob can view alice's timeline"
    (let [state-a {}
          time-now (t/now)
          state-b {"alice" {:timeline [{:message "a message"
                                        :author "alice"
                                        :timestamp time-now}]}}]
      (is (= []
             (view state-a "alice")))

      (is (= [{:message "a message"
               :author "alice"
               :timestamp time-now}]
             (view state-b "alice"))))))

(deftest reading-rec-test
  (testing "bob can view alice's timeline"
    (let [state-a {}
          time-now (t/now)
          timeline-entry (->TimelineEntry
                          "a message"
                          "alice"
                          (t/now))
          state-b (new-world [(new-user "alice" timeline-entry)])]
      (is (= []
             (view-rec state-a "alice")))

      (is (= [timeline-entry]
             (view-rec state-b "alice"))))))

(deftest subscribe-test
  (let [state {"charlie" {:timeline [] :subscriptions #{}}}
        time-now (t/now)
        state-a {"charlie" {:timeline [] :subscriptions #{"alice"}}
                 "alice" {:timeline [{:author "alice" :message "a message" :timestamp time-now}] :subscriptions #{}}}
        jan-1-2015 (t/date-time 2015 01 01)
        feb-1-2015 (t/date-time 2015 02 01)
        state-b {"charlie" {:timeline [] :subscriptions #{"alice" "bob"}}
                 "alice" {:timeline [{:author "alice"
                                      :message "a message"
                                      :timestamp time-now}] :subscriptions #{}}
                 "bob" {:timeline [{:message "bob's msg2"
                                    :author "bob"
                                    :timestamp feb-1-2015}
                                   {:message "bob's msg1"
                                    :author "bob"
                                    :timestamp jan-1-2015}
                                   ]}}]
    (testing "charlie can subscribe to alice's timeline"
      (is (= (update-in state ["charlie" :subscriptions] conj "alice")
             (subscribe state "charlie" "alice"))))
    (testing "charlie sees alice's timeline"
      (is (= []
             (feed state "charlie")))
      (let [result (feed state-a "charlie")]
        (are [x y] (= x y)
             "alice" (get-in result [0 :author])
             "a message" (get-in result [0 :message]))
        (is (t/within? (t/interval time-now (t/plus time-now (t/seconds 1)))
                       (get-in result [0 :timestamp])))))
    (testing "charlie can subscribe to alice and bob's timeline"
      (is (= (assoc-in state ["charlie" :subscriptions] #{"alice" "bob"})
             (-> state
                 (subscribe "charlie" "alice")
                 (subscribe "charlie" "bob"))))
      (is (= [{:author "alice" :message "a message" :timestamp time-now}
              {:author "bob" :message "bob's msg2" :timestamp feb-1-2015}
              {:author "bob" :message "bob's msg1" :timestamp jan-1-2015}]
             (feed state-b "charlie"))))))

(deftest subscribe-rec-test
  (let [state
        (new-world [(new-user "charlie")])
        time-now (t/now)
        state-a {"charlie"
                 (map->new-user
                  {:username "charlie"
                   :timeline []
                   :subscriptions #{"alice"}})
                 "alice"
                 (map->new-user
                  {:username "alice"
                   :timeline [(map->TimelineEntry
                               {:author "alice"
                                :message "a message"
                                :timestamp time-now})]
                   :subscriptions #{}})}
        jan-1-2015 (t/date-time 2015 01 01)
        feb-1-2015 (t/date-time 2015 02 01)
        state-b {"charlie"
                 (map->new-user
                  {:username "charlie"
                   :timeline []
                   :subscriptions #{"alice" "bob"}})
                 "alice"
                 (map->new-user
                  {:username "alice"
                   :timeline [(map->TimelineEntry
                               {:author "alice"
                                :message "a message"
                                :timestamp time-now})]})
                 "bob"
                 (map->new-user
                  {:username "bob"
                   :timeline [
                              (map->TimelineEntry
                               {:message "bob's msg2"
                                :author "bob"
                                :timestamp feb-1-2015})
                              (map->TimelineEntry
                               {:message "bob's msg1"
                                :author "bob"
                                :timestamp jan-1-2015})
                              ]})}]
    (testing "charlie can subscribe to alice's timeline"
      (is (= (update-in state ["charlie" :subscriptions] conj "alice")
             (subscribe-rec state "charlie" "alice"))))
    (testing "charlie sees alice's timeline"
      (is (= []
             (feed-rec state "charlie")))
      (let [result (feed-rec state-a "charlie")]
        (are [x y] (= x y)
             "alice" (get-in result [0 :author])
             "a message" (get-in result [0 :message]))
        (is (t/within? (t/interval time-now (t/plus time-now (t/seconds 1)))
                       (get-in result [0 :timestamp])))))
    (testing "charlie can subscribe to alice and bob's timeline"
      (is (= (assoc-in state ["charlie" :subscriptions] #{"alice" "bob"})
             (-> state
                 (subscribe-rec "charlie" "alice")
                 (subscribe-rec "charlie" "bob"))))
      (is (= [(map->TimelineEntry
               {:author "alice" :message "a message" :timestamp time-now})
              (map->TimelineEntry
               {:author "bob" :message "bob's msg2" :timestamp feb-1-2015})
              (map->TimelineEntry
               {:author "bob" :message "bob's msg1" :timestamp jan-1-2015})]
             (feed-rec state-b "charlie"))))))

(deftest extract-mentions-test
  "test mentions are parsed from message"
  (let [message "hello @chris"]
    (is (= #{"chris"}
           (extract-mentions message))))
  (let [message "hello @chris, @thomas and another @chris"]
    (is (= #{"chris" "thomas"}
           (extract-mentions message)))))

(deftest mentions-in-message-test
  "test mentions in message"
  (let [message "this message mentions @chris, @agile_geek, @thomas and @jr0cket"
        time-now (t/now)
        state {"Alice"
               {:timeline [{:author "Alice"
                            :message "message"
                            :timestamp time-now}]
                :mentions #{}
                :subscriptions #{}}}]
    (is (= #{"chris" "agile_geek" "thomas" "jr0cket"}
           (-> (publish state "Alice" message)
               (get-in ["Alice" :mentions]))))))

(deftest mentions-in-message-rec-test
  "test mentions in message"
  (let [message "this message mentions @chris, @agile_geek, @thomas and @jr0cket"
        time-now (t/now)
        state {"Alice"
               (new-user "Alice" (map->TimelineEntry
                                  {:message "message"
                                   :author "Alice"
                                   :timestamp time-now}))}]
    (is (= #{"chris" "agile_geek" "thomas" "jr0cket"}
           (-> (publish-rec state "Alice" message)
               (get-in ["Alice" :mentions]))))))

(deftest view-timeline-including-follows-test
  "View a users timeline including the timelines they follow"
  (let [time-now (t/now)
        jan-1-2015  (t/date-time 2015 01 01)
        jan-10-2015 (t/date-time 2015 01 10)
        feb-1-2015  (t/date-time 2015 02 01)
        feb-5-2015  (t/date-time 2015 02 05)
        jan-31-2015 (t/date-time 2015 01 31)
        state {"Alice"
               {:timeline [{:author "Alice" :message "Hello from Alice" :timestamp time-now}
                           {:author "Alice" :message "Another message from Alice" :timestamp feb-1-2015}
                           {:author "Alice" :message "I am drinking coffee." :timestamp jan-1-2015}]
                :subscriptions #{"Bob" "Charlie"}}
               "Bob"
               {:timeline [
                           {:author "Bob" :message "This is Bob's second post" :timestamp feb-5-2015}
                           {:author "Bob" :message "This is Bob's first post" :timestamp jan-10-2015}]}
               "Charlie"
               {:timeline [{:author "Charlie" :message "Hello from the Clojure Dojo" :timestamp jan-31-2015}]}}]
    (is (=
         [{:author "Alice" :message "Hello from Alice" :timestamp time-now}
          {:author "Bob" :message "This is Bob's second post" :timestamp feb-5-2015}
          {:author "Alice" :message "Another message from Alice" :timestamp feb-1-2015}
          {:author "Charlie" :message "Hello from the Clojure Dojo" :timestamp jan-31-2015}
          {:author "Bob" :message "This is Bob's first post" :timestamp jan-10-2015}
          {:author "Alice" :message "I am drinking coffee." :timestamp jan-1-2015}]
         (view-all state "Alice")))))

(deftest view-timeline-including-follows-rec-test
  "View a users timeline including the timelines they follow"
  (let [time-now (t/now)
        jan-1-2015  (t/date-time 2015 01 01)
        jan-10-2015 (t/date-time 2015 01 10)
        feb-1-2015  (t/date-time 2015 02 01)
        feb-5-2015  (t/date-time 2015 02 05)
        jan-31-2015 (t/date-time 2015 01 31)
        state (new-world
               [(map->new-user
                 {:username "Alice"
                  :timeline [(map->TimelineEntry
                              {:author "Alice" :message "Hello from Alice" :timestamp time-now})
                             (map->TimelineEntry
                              {:author "Alice" :message "Another message from Alice" :timestamp feb-1-2015})
                             (map->TimelineEntry
                              {:author "Alice" :message "I am drinking coffee." :timestamp jan-1-2015})]
                  :subscriptions #{"Bob" "Charlie"}})
                (map->new-user
                 {:username "Bob"
                  :timeline [(map->TimelineEntry
                              {:author "Bob" :message "This is Bob's second post" :timestamp feb-5-2015})
                             (map->TimelineEntry
                              {:author "Bob" :message "This is Bob's first post" :timestamp jan-10-2015})]})
                (map->new-user
                 {:username "Charlie"
                  :timeline [(map->TimelineEntry
                              {:author "Charlie" :message "Hello from the Clojure Dojo" :timestamp jan-31-2015})]})])]
    (is (=
         [(map->TimelineEntry {:author "Alice" :message "Hello from Alice" :timestamp time-now})
          (map->TimelineEntry {:author "Bob" :message "This is Bob's second post" :timestamp feb-5-2015})
          (map->TimelineEntry {:author "Alice" :message "Another message from Alice" :timestamp feb-1-2015})
          (map->TimelineEntry {:author "Charlie" :message "Hello from the Clojure Dojo" :timestamp jan-31-2015})
          (map->TimelineEntry {:author "Bob" :message "This is Bob's first post" :timestamp jan-10-2015})
          (map->TimelineEntry {:author "Alice" :message "I am drinking coffee." :timestamp jan-1-2015})]
         (view-all-rec state "Alice")))))
