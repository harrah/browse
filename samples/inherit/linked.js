$(function() {
	$("[id]").live("mouseover",
		function() { $(this).references().highlight() }
	).live("mouseout",
		function() { $(this).references().unhighlight() }
	)
	$("a[href^='#']").live("mouseover",
		function() { $(this).definition().highlight() }
	).live("mouseout",
		function() { $(this).definition().unhighlight() }
	)
})

jQuery.fn.extend({
	definition: function() {
		return $("#" + /#(\d+)$/.exec(this.attr('href'))[1])
	},
	references: function() {
		return $("a[href$='#" + this.attr('id') +"']")
	},
	highlight:	function() {
		return this.addClass("highlighted");
	},
	unhighlight: function() {
		return this.removeClass("highlighted");
	}
})
