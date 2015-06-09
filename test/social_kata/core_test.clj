(ns social-kata.core-test
  (:require [clojure.test :refer :all]
            [social-kata.core :refer :all]))

(deftest a-test
  (testing "Alice can publish to a personal timeline"
    (is (= {"alice" [{:message "hello world." :mentions #{}}]}
           (publish {} "alice" "hello world.")))))

(deftest reading-test
  (testing "bob can view alice's timeline"
    (let [state-a {}
          state-b {"alice" {:timeline ["a message"]}}]
      (is (= []
             (view state-a "alice")))

      (is (= ["a message"]
             (view state-b "alice"))))))

(deftest subscribe-test
  (let [state {"charlie" {:timeline [] :subscriptions #{}}}
        state-a {"charlie" {:timeline [] :subscriptions #{"alice"}}
                 "alice" {:timeline [{:author "alice" :message "a message"}] :subscriptions []}}
        state-b {"charlie" {:timeline [] :subscriptions #{"alice" "bob"}}
                 "alice" {:timeline [{:author "alice" :message
                                      "a message"}] :subscriptions #{}}
                 "bob" {:timeline [{:message "bob's msg1"
                                    :author "bob"}
                                   {:message "bob's msg2"
                                    :author "bob"}]}}]
    (testing "charlie can subscribe to alice's timeline"
      (is (= (update-in state ["charlie" :subscriptions] conj "alice")
             (subscribe state "charlie" "alice"))))
    (testing "charlie sees alice's timeline"
      (is (= []
             (feed state "charlie")))
      (is (= [{:author "alice" :message "a message"}]
             (feed state-a "charlie"))))
    (testing "charlie can subscribe to alice and bob's timeline"
      (is (= (assoc-in state ["charlie" :subscriptions] #{"alice" "bob"})
             (-> state
                 (subscribe "charlie" "alice")
                 (subscribe "charlie" "bob"))))
      (is (= [{:author "bob" :message "bob's msg1"}
              {:author "bob" :message "bob's msg2"}
              {:author "alice" :message "a message"}]
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
        state {"Alice"
               {:timeline [{:author "Alice" :message []}]
                :mentions #{}
                :subscriptions []}}]
    (is (= #{"chris" "agile_geek" "thomas" "jr0cket"}
           (-> (publish state "Alice" message)
               (get-in ["Alice" :mentions]))))))
