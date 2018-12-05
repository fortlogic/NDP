# -*- mode: ruby -*-
# vi: set ft=ruby :

# All Vagrant configuration is done below. The "2" in Vagrant.configure
# configures the configuration version (we support older styles for
# backwards compatibility). Please don't change it unless you know what
# you're doing.
Vagrant.configure(2) do |config|
  # The most common configuration options are documented and commented below.
  # For a complete reference, please see the online documentation at
  # https://docs.vagrantup.com.

  config.vm.box = "ubuntu/trusty64"  # This one doesn't seem to have CDC_ACM module :(
  # config.vm.box = "hashicorp/precise64"  # Going with older one because it has serial drivers

  config.vm.hostname = "NDP-xilinx-host"

  config.vm.synced_folder ENV['XILINXROOT'], "/xilinx"

  config.vm.provision :shell, path: (ENV['XILINXROOT'] + "/provision-vm.sh")

  config.ssh.forward_x11 = true

  # Add usb filter
  config.vm.provider :virtualbox do |v|

    # Set how much memory and CPU cores to use
    v.memory = 1024
    v.cpus = 2

    # Enable USB
    v.customize ["modifyvm", :id, "--usb", "on"]
    v.customize ['usbfilter', 'add', '0', '--target', :id, '--name', 'Saanlima Pipistrello LX45 [0700]', '--manufacturer', 'Saanlima', '--product', 'Pipistrello LX45']
  end
end
