function load(path, to)
{
	var xmlhttp;
	if (window.XMLHttpRequest) { xmlhttp=new XMLHttpRequest(); }
	else { xmlhttp=new ActiveXObject("Microsoft.XMLHTTP"); }
	xmlhttp.onreadystatechange=function()
	{
		if (xmlhttp.readyState==4 && xmlhttp.status==200)
		{
			document.getElementById(to).innerHTML=xmlhttp.responseText;
		}
	};
	xmlhttp.open("GET", path, true);
	xmlhttp.send();
}

function move(direction)
{
	load('/script/handle%3Aevent?move=' + direction, 'map');
	load('/script/handle%3Astats', 'stats');
}

function act()
{
	load('/script/handle%3Aevent?action=act', 'map');
	load('/script/handle%3Astats', 'stats');
}

function logined()
{
	load('/script/handle%3Amap', 'map');
	load('/script/handle%3Astats', 'stats');
	// setInterval(function(){update_map()}, 1000);
}

function update_map()
{
	load('/script/handle%3Amap', 'map');
}

function send_msg(form)
{
	load('/script/handle%3Aevent?msg=' + form.msg.value, 'map');
	form.msg.value = '';
	load('/script/handle%3Astats', 'stats');
}

function login(form)
{
	load("/script/handle%3Alogin?u=" + form.u.value + "&p=" + form.p.value, "mainframe");
}
