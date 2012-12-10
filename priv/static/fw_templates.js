$(document).ready(function() {
	$('#fw_template_form').submit(function(){
		var url;
		var exists = $('input[name=revision]').length != 0;

		if (exists){
			url = window.location.pathname;
		} else{
			url = window.location.pathname + '/' + $('[name=name]', this).val();
		}
		var self = this;
		$(this).ajaxSubmit({
			url: url,
			error: function(xhr){
				$('p.alert').remove();
				var errors = $.parseJSON(xhr.responseText);
				for (name in errors){
					$('<p class="alert alert-error">' + errors[name] + '</p>')
						.insertBefore('[name=' + name + ']');
				}
			},
			success: function(){
				$('p.alert').remove();
				if (exists){
					$('h3').before('<p class="alert alert-success">Success! Feel free to continue editing.</p>');
				} else{
					$('h3').before('<p class="alert alert-success">Success! <a href="' + url + '">Jump to the created template</a> or create another!</p>');
					self.reset();
				}
				$("p.alert").delay(2000).fadeOut("slow", function(){$(this).remove();});
			}
		});

		return false;
	});

	$('a.delete').bind('click', function(event){
		event.preventDefault();
		$.ajax({
			type: 'DELETE',
			url: this.href
		});
	});
});
