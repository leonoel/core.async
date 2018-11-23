(ns clojure.core.async.ioc-macros-test
  (:refer-clojure :exclude [map into reduce transduce merge take partition
                            partition-by])
  (:require [clojure.core.async :refer :all :as async]
            [clojure.test :refer :all]))

(defn identity-chan
  "Defines a channel that instantly writes the given value"
  [x]
  (let [c (chan 1)]
    (>!! c x)
    (close! c)
    c))

(deftest async-test
  (testing "values are returned correctly"
    (is (= 10
           (<!! (go (<! (identity-chan 10)))))))
  (testing "writes work"
    (is (= 11
           (<!! (go (let [c (chan 1)]
                      (>! c (<! (identity-chan 11)))
                      (<! c)))))))

  (testing "case with go"
    (is (= :1
           (<!! (go (case (name :1)
                      "0" :0
                      "1" :1
                      :3))))))

  (testing "nil result of go"
    (is (= nil
           (<!! (go nil)))))

  (testing "take inside binding of loop"
    (is (= 42
           (<!! (go (loop [x (<! (identity-chan 42))]
                      x))))))

  (testing "can get from a catch"
    (let [c (identity-chan 42)]
      (is (= 42
             (<!! (go (try
                        (assert false)
                        (catch Throwable ex (<! c))))))))))

(deftest offer-poll
  (let [c (chan 2)]
    (is (= [true true 5 6 nil]
           (<!! (go [(offer! c 5) (offer! c 6) (poll! c) (poll! c) (poll! c)]))))))

(deftest enqueued-chan-ops
  (testing "enqueued channel puts re-enter async properly"
    (is (= [:foo 42]
           (let [c (chan)
                 result-chan (go (>! c :foo) 42)]
             [(<!! c) (<!! result-chan)]))))
  (testing "enqueued channel takes re-enter async properly"
    (is (= :foo
           (let [c (chan)
                 async-chan (go (<! c))]
             (>!! c :foo)
             (<!! async-chan)))))
  (testing "puts into channels with full buffers re-enter async properly"
    (is (= #{:foo :bar :baz :boz}
           (let [c (chan 1)
                 async-chan (go
                              (>! c :foo)
                              (>! c :bar)
                              (>! c :baz)

                              (>! c :boz)
                              (<! c))]
             (set [(<!! c)
                   (<!! c)
                   (<!! c)
                   (<!! async-chan)]))))))

(deftest alt-tests
  (testing "alts works at all"
    (let [c (identity-chan 42)]
      (is (= [42 c]
             (<!! (go (alts!
                        [c])))))))
  (testing "alt works"
    (is (= [42 :foo]
           (<!! (go (alt!
                      (identity-chan 42) ([v] [v :foo])))))))

  (testing "alts can use default"
    (is (= [42 :default]
           (<!! (go (alts!
                      [(chan 1)] :default 42))))))

  (testing "alt can use default"
    (is (= 42
           (<!! (go (alt!
                      (chan) ([v] :failed)
                      :default 42))))))

  (testing "alt obeys its random-array initialization"
    (is (= #{:two}
           (with-redefs [clojure.core.async/random-array
                         (constantly (int-array [1 2 0]))]
             (<!! (go (loop [acc #{}
                             cnt 0]
                        (if (< cnt 10)
                          (let [label (alt!
                                        (identity-chan :one) ([v] v)
                                        (identity-chan :two) ([v] v)
                                        (identity-chan :three) ([v] v))]
                            (recur (conj acc label) (inc cnt)))
                          acc)))))))))

(deftest close-on-exception-tests
  (testing "threads"
    (is (nil? (<!! (thread (assert false "This exception is expected")))))
    (is (nil? (<!! (thread (alts!! [(identity-chan 42)])
                           (assert false "This exception is expected"))))))
  (testing "go blocks"
    (is (nil? (<!! (go (assert false "This exception is expected")))))
    (is (nil? (<!! (go (alts! [(identity-chan 42)])
                       (assert false "This exception is expected")))))))

(deftest resolution-tests
  (let [<! (constantly 42)]
    (is (= 42 (<!! (go (<! (identity-chan 0)))))
        "symbol translations do not apply to locals outside go"))

  (is (= 42 (<!! (go (let [<! (constantly 42)]
                       (<! (identity-chan 0))))))
      "symbol translations do not apply to locals inside go")

  (let [for vector x 3]
    (is (= [[3 [0 1]] 3]
           (<!! (go (for [x (range 2)] x))))
        "locals outside go are protected from macroexpansion"))

  (is (= [[3 [0 1]] 3]
         (<!! (go (let [for vector x 3]
                    (for [x (range 2)] x)))))
      "locals inside go are protected from macroexpansion")

  (let [c (identity-chan 42)]
    (is (= [42 c] (<!! (go (async/alts! [c]))))
        "symbol translations apply to resolved symbols")))

(deftest go-nests
  (is (= [23 42] (<!! (<!! (go (let [let* 1 a 23] (go (let* [b 42] [a b])))))))))

(defprotocol P
  (x [p]))

(defrecord R [z]
  P
  (x [this]
    (go
      (loop []
        (if (zero? (rand-int 3))
          [z (.z this)]
          (recur))))))

(deftest go-propagates-primitive-hints
  (is (= "asd" (<!! (let [a (int 1)] (go (.substring "fasd" a))))))
  (is (= 1 (<!! (let [a (int 1)] (go (Integer/valueOf a))))))
  (is (= [1 1] (<!! (x (R. 1))))))

(deftest ASYNC-186
  (is (let [y nil] (go))))
