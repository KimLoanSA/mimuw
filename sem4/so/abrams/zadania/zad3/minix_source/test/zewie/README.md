użycie:
```bash
#na minixe:
make test-basic
echo <seed> | ./test-basic
```

Czekamy aż się skończy i `ps ax | grep test-basic` nic nie wypisze poza samym sobą.
Może zająć z minutę.

ściągamy output.txt

```bash
#na hoście:
./change.py output.txt >myRes<seed>
diff res<seed> myRes<seed>
```
