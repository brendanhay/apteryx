Vagrant.configure('2') do |config|
  config.vm.box_url = 'https://cloud-images.ubuntu.com/vagrant/trusty/current/trusty-server-cloudimg-amd64-vagrant-disk1.box'
  config.vm.box     = 'trusty-server'

 config.ssh.forward_agent = true

 config.vm.provision :shell, :inline => $bootstrap

  config.vm.provider(:virtualbox) do |vb|
    vb.customize ["modifyvm", :id, "--memory", "8096"]
    vb.customize ["modifyvm", :id, "--cpus", "2"]
  end

  config.vm.synced_folder ".", "/home/vagrant/apteryx"
end

$bootstrap = <<-SCRIPT
# updated=~/.apt-updated

if [ ! -f $updated ]; then apt-get update && touch $updated; fi
if [ ! -f $upgraded ]; then apt-get upgrade && touch $upgrade; fi

apt-get install -y \
 build-essential \
 man \
 git-core \
 zlib1g-dev
SCRIPT
