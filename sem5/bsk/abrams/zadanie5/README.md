# Rozwiązanie


## Nowe CA

### Klucz prywatny CA - `ca.key`
```
openssl genrsa -aes256 -out ca.key 4096
```
password: `password`

### Certyfikat CA - `ca.crt`
```
openssl req -new -x509 -days 2920 -key ca.key -out ca.crt
```


## Prośba o certyfikat
```
openssl genrsa -out bsk.key 4096
openssl req -new -key bsk.key -out bsk.csr
```

## Wystawianie certyfikatu
```
openssl x509 -days 547 -req -in bsk.csr -CA ca.crt -CAkey ca.key -CAcreateserial -out bsk.crt
```

# Uruchamianie
```
./buildAndRunDocker.sh
```

# W przeglądarce:

## Import certyfikatu:
Dla mac OS: [link](https://www.keokukschools.org/certificate-safari-chrome/)

## Zaktualizować `etc/hosts`:
```
...
127.0.0.1       prac.gfb.intra
...
```
## Wpisać url:
https://prac.gfb.intra
