make build

for f in test/samples/fail*
do
    echo $f
    dune exec _build/default/test/semant_test.exe $f 2> /dev/null
done