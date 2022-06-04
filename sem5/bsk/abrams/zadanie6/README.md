## Rozwiązanie

1. Base image:
    ```
    cd docker/base
    ./buildBaseImage.sh
    ```
2. Uruchamianie `docker-compose`:
    ```
    cd ..
    ./runDockerCompose.sh
    ```

3. `firewall`:
    ```
    ./setup
    ```
    hasło: `haslo`


4. `www`:
    ```
    ./setup
    ```
  
5. `manager`:
    ```
    ./setup
    ```
    hasło: `haslo`
    
    i można sprawdzić:
    ```
    curl 10.32.0.6
   ```
