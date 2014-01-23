function fullUri(ref, query) {
	var loc = /^([^\?#]+)/.exec(location.href)[1];
	return ref ? loc + (query ? "?id=" : "#") + $(ref).attr("id") : loc;
}
function updateExportUri(ref) {
	$("#export span.src").text(fullUri(ref, true))
}
var last_def = false;
function scrollTo(def) {
	$(window).scrollTo( def, 300, { 
		onAfter: function() {
			if (location.search.match(/^\?id=/)) {
				if (last_def)
					last_def.unhighlight();
				def.highlight();
			} else {
				document.location = "#" + def.attr("id");
			}
			last_def = def;
			updateExportUri(def);
		}, axis: "y"
	});
	return false;
}
$(function() {
	$("pre [id]").on("mouseover",
		function() { $(this).references().highlight() }
	).on("mouseout",
		function() { $(this).references().unhighlight() }
	).on("click", function() { $(this).attr("href") ? false : scrollTo($(this)) });
	
	$("pre a[href^='#']").on("mouseover",
		function() { $(this).definition().highlight() }
	).on("mouseout",
		function() { $(this).definition().unhighlight() }
	).on("click",
		function() { return scrollTo($(this).definition()); }
	);
});


$(document).ready(function () {
    $('[title!=""]').qtip({
        style: {
            classes: 'qtip-dark qtip-rounded',
            tip: true,
            width: { max: 500 }
        },
        hide: {
            event: false,
            inactive: 2000
        },
        show: {
            solo: true,
            delay: { length: 0 }
        }
    });
});

$(window).load(function() {
	var id = id = /^\?id=(.+)$/.exec(location.search);
	
	if(top != window) { // if showing in frame
		if (id) scrollTo($("#" + id[1]));

		// display info bar when mouse in frame
		var pop_out = $('<div class="tool" id="pop-out"><input type="submit" value="Pop Out" />Source published by Scala X-Ray</div>')
		$("body").append(pop_out);
		$(window.document).hover(
			function() { pop_out.stop(false, true); pop_out.fadeIn() },
			function() { pop_out.stop(false, true); pop_out.fadeOut() }
		);
		pop_out.find("input").click(function() {
			window.open(fullUri(last_def));
			return false;
		});
	} else { // if not showing in a frame
		if (id)	// redirect to #id if given id param
			window.location = fullUri($("#" + id[1]));

		var exp = $(
			'<div class="tool" id="export">' +
			'<code>&lt;iframe src="<span class="src">path-to-source</span>" width="<span class="width">700</span>"' +
			' height="<span class="height">500</span>" frameborder="0"&gt; &lt;/iframe&gt;</code>' +
			'</div>'
		);
		var exp_control = $('<div class="tool" id="export-control"><a href="#">Export</a></div>');
		$("body").append(exp).append(exp_control);
		
		updateExportUri(location.hash);
		exp_control.show().click( function() {
			exp.slideToggle("fast");
			return false;
		});
		$(window).resize(sizeUpdate = function() {
			exp.find("span.width").text(window.innerWidth || document.documentElement.clientWidth);
			exp.find("span.height").text(window.innerHeight || document.documentElement.clientWidth);
		});
		sizeUpdate();
	}
});

jQuery.fn.extend({
	definition: function() {
		var target = document.getElementById(this.attr("href").replace(/^#/, ''));
		return target ? $(target) : $(this);
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
