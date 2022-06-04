FROM nginx:latest

COPY config/ca/bsk.crt /etc/nginx/ssl/bsk.crt
COPY config/ca/bsk.key  /etc/nginx/ssl/bsk.key

COPY config/ssl/nginx.conf /etc/nginx/nginx.conf
COPY config/ssl/html/index.html /etc/nginx/html/index.html
