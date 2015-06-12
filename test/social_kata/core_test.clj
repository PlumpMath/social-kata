(ns social-kata.core-test
  (:require [clojure.test :refer :all]
            [social-kata.core :refer :all]
            [clj-time.core :as t]))

(deftest a-test
  (testing "Alice can publish to a personal timeline"
    (is (= {"alice" [{:message "hello world." :mentions #{}}]}
           (publish {} "alice" "hello world.")))))

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
