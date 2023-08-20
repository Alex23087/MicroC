make build

for f in test/samples/test*
do
    dune exec _build/default/test/parser_test.exe $f 1> /dev/null
done