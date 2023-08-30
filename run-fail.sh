make build

for f in test/samples/fail*
do
    echo $f
    dune exec _build/default/test/codegen_test.exe $f 2> /dev/null
done