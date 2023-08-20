make build

for f in test/samples/fail*
do
    dune exec _build/default/test/parser_test.exe $f 2> /dev/null
done