{pkgs, ...}: {
  home.packages = [pkgs.syncthing];

  # TODO: how to use nix-darwin's services.emacs here?
  #launchd.user.agents.syncthing = {
  #  command =  "${pkgs.syncthing}/bin/syncthing serve --no-browser";
  #  serviceConfig.KeepAlive = true;
  #  serviceConfig.RunAtLoad = true;
  #};
}
