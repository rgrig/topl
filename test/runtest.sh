cd test
javac TreapTest.java
cd ..
./toplc.native -i test -o test_instrumented test/treaptest.topl
cd test_instrumented/
javac topl/PropertyToDOT.java
java topl/PropertyToDOT
dot -Tpdf -oProperty.pdf Property.dot
java TreapTest
