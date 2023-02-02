picom -fb &
nitrogen --restore &

exec dbus-launch --exit-with-session emacs -mm --debug-init
