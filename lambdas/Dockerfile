# This container is needed to build the native Canvas code for Amazon linux used in PGN rendering lambdas
FROM amazonlinux:latest

# set locale
RUN echo LC_ALL=en_GB.UTF-8 >> /etc/environment
ENV LC_ALL=en_GB.UTF-8

# install node and build tools
RUN curl --silent --location https://rpm.nodesource.com/setup_12.x | bash - && \
    yum install -y nodejs gcc-c++ make git libuuid1 cairo-devel libjpeg-turbo-devel giflib-devel pango-devel libfontconfig1
    

# run container as 
# docker run --rm -it -v -v "/path/to/pigeomatic:/src" -v "/path/to/pigeomatic/lambdas/node_modules:/src/node_modules" canvas-dev:latest
# In Windows I run it like this:
# docker run --rm -it -v "$env:Userprofile\Documents\development\pigeomatic\:/src" -v "$env:Userprofile\Documents\development\pigeomatic\lambdas\node_modules:/src/node_modules" canvas-dev:latest

# After a npm install you will have an Amazon linux Canvas build that can be used in lambdas. 
# You can now go back to your development in Windows/Mac keeping in mind that Canvas is not gonna work locally
# as it is native linux code. You can always go back into the container and do your rendering from there...
# *I still needed to copy the following deps to get Canvas working
#cp /usr/lib64/libuu* node_modules/canvas/build/Release
#cp /usr/lib64/libmount* node_modules/canvas/build/Release
#cp /usr/lib64/libblkid* node_modules/canvas/build/Release
