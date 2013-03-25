# -*- mode: ruby -*-
# vi: set ft=ruby :

# configure this
CHEF_KITCHEN = '/path/to/your/chef/kitchen'

Vagrant.configure("2") do |config|
  config.vm.box = "precise64"
  config.vm.box_url = "http://files.vagrantup.com/precise64.box"

  config.vm.network :forwarded_port, guest: 80, host: 8080

  config.vm.provider :virtualbox do |vb|
    vb.customize ["modifyvm", :id, "--memory", "1024"] # ghc is memory hungry
  end

  config.vm.provision :chef_solo do |chef|
    chef.cookbooks_path = "#{CHEF_KITCHEN}/cookbooks"
    chef.roles_path = "#{CHEF_KITCHEN}/roles"
    chef.add_role 'yesodoro_app'
  end
end
