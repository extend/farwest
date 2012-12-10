#!/usr/bin/env sh

DEPS_DIR=priv/deps

# boostrap
BOOTSTRAP_ARCHIVE=bootstrap.zip
BOOTSTRAP_DIR=$DEPS_DIR/bootstrap
BOOTSTRAP_URL=http://twitter.github.com/bootstrap/assets/$BOOTSTRAP_ARCHIVE

# jquery
JQUERY_DIR=$DEPS_DIR/jquery
JQUERY_URL=http://code.jquery.com/jquery-1.7.2.min.js

# jquery plugins
JQ_FORM_URL=https://raw.github.com/malsup/form/5904d38c2636bf037c8328aab6576361af53da75/jquery.form.js
JQ_JSON_URL=todo

case "$1" in
	get)
		mkdir -p $DEPS_DIR
		# bootstrap
		if [ ! -e $BOOTSTRAP_DIR ]; then
			wget --output-document=$BOOTSTRAP_ARCHIVE $BOOTSTRAP_URL
			unzip -d $DEPS_DIR $BOOTSTRAP_ARCHIVE
			rm $BOOTSTRAP_ARCHIVE
		fi
		# jquery
		if [ ! -e $JQUERY_DIR/jquery.min.js ]; then
			mkdir -p $JQUERY_DIR
			wget --output-document=$JQUERY_DIR/jquery.min.js $JQUERY_URL
		fi
		# jquery plugins
		if [ ! -e $JQUERY_DIR/jquery.form.js ]; then
			wget --output-document=$JQUERY_DIR/jquery.form.js $JQ_FORM_URL
		fi
		if [ ! -e $JQUERY_DIR/jquery.json.js ]; then
			wget --output-document=$JQUERY_DIR/jquery.json.js $JQ_JSON_URL
		fi
		;;
	delete)
		rm -rf $DEPS_DIR
		;;
esac

exit 0
