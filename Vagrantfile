# -*- mode: ruby -*-
# vi: set ft=ruby :

KETER_ROOT = '/var/keter'

# configure these
CHEF_KITCHEN = '/path/to/your/chef/kitchen'
CHEF_ENCRYPTED_DATABAG_SECRET = '~/.chef/encrypted_data_bag_secret'

Vagrant.configure("2") do |config|
  config.vm.box = "precise64"
  config.vm.box_url = "http://files.vagrantup.com/precise64.box"

  config.vm.network :forwarded_port, guest: 80, host: 8080
  config.vm.network :forwarded_port, guest: 443, host: 44380

  config.vm.synced_folder 'keter', KETER_ROOT

  config.vm.provider :virtualbox do |vb|
    vb.customize ["modifyvm", :id, "--memory", "1024"] # ghc is memory hungry
  end

  config.vm.provision :chef_solo do |chef|
    chef.cookbooks_path = "#{CHEF_KITCHEN}/cookbooks"
    chef.roles_path = "#{CHEF_KITCHEN}/roles"
    chef.data_bags_path = "#{CHEF_KITCHEN}/data_bags"
    chef.encrypted_data_bag_secret_key_path = CHEF_ENCRYPTED_DATABAG_SECRET

    chef.add_role 'yesodoro_app'

    chef.json = {
      keter: {
        version: '0.3.6.1',
        root: KETER_ROOT,
      },
    }
  end
end
