make build

for f in test/samples/test*.mc
do
    dune exec _build/default/test/semant_test.exe $f 1> /dev/null
done