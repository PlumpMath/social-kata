(ns social-kata.core-test
  (:require [clojure.test :refer :all]
            [social-kata.core :refer :all]))

(deftest a-test
  (testing "Alice can publish to a personal timeline"
    (is (= {"Alice" ["Hello world."]} 
           (publish {} "Alice" "Hello world.")))))

(deftest reading-test
  (testing "Bob can view Alice's timeline"
    (let [state-a {}
          state-b {"Alice" ["A message"]}]
      (is (= []
             (view state-a "Alice")))

      (is (= ["A message"]
             (view state-b "Alice"))))))

(deftest subscribe-test
  (let [state {"Charlie" {:timeline [] :subscriptions []}}
        state-a {"Charlie" {:timeline [] :subscriptions []}
               "Alice" {:timeline ["A message"] :subscriptions []}}]
    (testing "Charlie can subscribe to Alice's timeline"
      (is (= (update-in state ["Charlie" :subscriptions] conj "Alice")
             (subscribe state "Charlie" "Alice"))))
    (testing "Charlie sees alice's timeline"
      (is (= []
             (feed state "Charlie")))
      (is (= []
             (feed state-a "Charlie"))))))
