(ns social-kata.core-test
  (:require [clojure.test :refer :all]
            [social-kata.core :refer :all]))

(deftest a-test
  (testing "Alice can publish to a personal timeline"
    (is (= {"Alice" [{:message "Hello world." :mentions #{}}]}
           (publish {} "Alice" "Hello world.")))))

(deftest reading-test
  (testing "Bob can view Alice's timeline"
    (let [state-a {}
          state-b {"Alice" {:timeline ["A message"]}}]
      (is (= []
             (view state-a "Alice")))

      (is (= ["A message"]
             (view state-b "Alice"))))))

(deftest subscribe-test
  (let [state {"Charlie" {:timeline [] :subscriptions []}}
        state-a {"Charlie" {:timeline [] :subscriptions ["Alice"]}
                 "Alice" {:timeline [{:author "Alice" :message "A message"}] :subscriptions []}}
        state-b {"Charlie" {:timeline [] :subscriptions ["Alice" "Bob"]}
                 "Alice" {:timeline [{:author "Alice" :message
                                      "A message"}] :subscriptions []}
                 "Bob" {:timeline [{:message "Bob's msg1"
                                    :author "Bob"}
                                   {:message "Bob's msg2"
                                    :author "Bob"}]}}]
    (testing "Charlie can subscribe to Alice's timeline"
      (is (= (update-in state ["Charlie" :subscriptions] conj "Alice")
             (subscribe state "Charlie" "Alice"))))
    (testing "Charlie sees alice's timeline"
      (is (= []
             (feed state "Charlie")))
      (is (= [{:author "Alice" :message "A message"}]
             (feed state-a "Charlie"))))
    (testing "Charlie can subscribe to Alice and Bob's timeline"
      (is (= (assoc-in state ["Charlie" :subscriptions] ["Alice" "Bob"])
             (-> state
                 (subscribe "Charlie" "Alice")
                 (subscribe "Charlie" "Bob"))))
      (is (= [{:author "Alice" :message "A message"}
              {:author "Bob" :message "Bob's msg1"}
              {:author "Bob" :message "Bob's msg2"}]
             (feed state-b "Charlie"))))))

(deftest extract-mentions-test
  (let [message "Hello @Chris"]
    (is (= #{"Chris"}
           (extract-mentions message)))))
