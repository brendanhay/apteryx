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
apt-get update
apt-get upgrade

apt-get install -y \
 build-essential \
 man \
 git-core \
 zlib1g-dev \
 alex \
 happy \
 ghc \
 cabal-install

su - vagrant -c "
echo 'export PATH=~/.cabal/bin:$PATH' >> ~/.bashrc
cabal update
cabal install cabal-install
cd /home/vagrant/apteryx && make
"
SCRIPT
