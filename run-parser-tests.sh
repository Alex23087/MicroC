make build

for f in test/samples/test*.mc
do
    dune exec _build/default/test/parser_test.exe $f 1> /dev/null
done