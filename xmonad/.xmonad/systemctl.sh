#!/bin/sh
mkdir -p ~/.config/systemd/user

cat <<EOF > ~/.config/systemd/user/plasma-xmonad.service
[Install]
WantedBy=plasma-workspace.target

[Unit]
Description=Plasma XMonad Window Manager
Before=plasma-workspace.target

[Service]
ExecStart=/usr/bin/xmonad
Slice=session.slice
Restart=on-failure
EOF

systemctl --user mask plasma-kwin_x11.service
systemctl --user daemon-reload
systemctl --user enable plasma-xmonad.service
