# Start Programs
picom -fb &

# Start Emacs
exec dbus-launch --exit-with-session emacs -mm --debug-init -l ~/.emacs.d/desktop.el
