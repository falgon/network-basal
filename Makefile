WORKDIR = _network-basal-workdir

test:
	mkdir -p ../$(WORKDIR)
	mv testenv ../$(WORKDIR)
	cd ../
	mv network-basal $(WORKDIR)/
	cd $(WORKDIR)
	vagrant up
	vagrant ssh -c 'sudo stack --allow-different-user --stack-yaml /vagrant/network-basal/stack.yaml --stack-root /vagrant test' node1
	vagrant halt
	cd ..
	mv $(WORKDIR)/provision.sh $(WORKDIR)/Vagrantfile $(WORKDIR)/network-basal/
	mv $(WORKDIR)/network-basal .
	rm -r $(WORKDIR)
	cd network-basal

.PHONY: test clean
