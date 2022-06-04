#!/bin/bash

function assert_found() {
  grep "$1" "$2" >/dev/null 2>/dev/null
  if [ $? = 0 ]; then
    echo OK
  else
    echo FAIL
  fi
}

logger --priority user.warning "final warning"
assert_found "final warning" /var/log/syslog

logger --priority user.notice "final notice"
assert_found "final notice" /var/log/notice.log

logger --priority mail.notice "YOU GOT MAIL"
assert_found "YOU GOT MAIL" /var/log/mail.log

logger --priority kern.warning "Linus Torvalds is an asshole"
assert_found "Linus Torvalds is an asshole" /var/log/kernel.log

logger --priority kern.notice "***** ***"
assert_found "***** ***" /var/log/syslogkern.log

date +%T -s "14:47:00"
logger "none of your business"
assert_found "none of your business" /var/log/business_banking.log

date +%T -s "21:37:00"
logger "business is business"
assert_found "business is business" /var/log/afterhours.log
