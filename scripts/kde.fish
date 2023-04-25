#!/usr/bin/fish
# https://wiki.archlinux.org/title/KDE#Backup_and_restore

set -l out $argv[1]

if [ -z "$out" ]
    echo "usage: kde.fish out.tar.gz"
    exit 2
end


begin
    pushd ~/.config

    # thanks https://github.com/shalva97/kde-configuration-files
    set files Trolltech.conf akregatorrc baloofilerc bluedevilglobalrc kactivitymanagerd-statsrc
    set -a files kactivitymanagerdrc kactivitymanagerd-pluginsrc kateschemarc kcmfonts kcminputrc kconf_updaterc kded5rc
    set -a files kdeglobals kfontinstuirc kglobalshortcutsrc khotkeysrc kmixctrlrc kmixrc
    set -a files kscreenlockerrc ksmserverrc ksplashrc ktimezonedrc kwinrc kwinrulesrc plasma-localerc
    set -a files plasma-nm plasma-org.kde.plasma.desktop-appletsrc plasmarc plasmashellrc
    set -a files powermanagementprofilesrc startupconfig startupconfigfiles startupconfigkeys
    set -a files krunnerrc touchpadxlibinputrc systemsettingsrc kxkbrc PlasmaUserFeedback
    set -a files kde.org/* kiorc klipperrc knfsshare kuriikwsfilterrc kwalletmanager5rc kwalletrc
    set -a files plasma.emojierrc plasmanotifyrc PlasmaUserFeedback powerdevilrc kgammarc
    set -a files kded_device_automounterrc device_automounter_kcmrc klaunchrc
    set -a files trashrc kactivitymanagerd-switcher gtkrc-2.0 gtkrc baloofileinformationrc
    set -a files breezerc

    echo "checking for" (count $files) "configuration files"
    set files (readlink -e $files 2>/dev/null)

    popd
end

echo "found" (count $files) "files, zipping $out..."
tar czf "$out" $files
rm $files
echo done
