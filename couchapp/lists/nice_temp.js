function format(o,name) {
    "use strict";

    send('<h2>');
    send(name);
    send('</h2><ul>');

    send('<li>Keskilämpötila ');
    send((o.sum / o.n).toFixed(1));
    send('°C (');
    send(o.n);
    send(' mittausta)</li>');

    send('<li>Minimi ');
    send(o.min.toFixed(1));
    send('°C (');
    send(fin_date(o.min_time));
    send(')</li>');

    send('<li>Maksimi ');
    send(o.max.toFixed(1));
    send('°C (');
    send(fin_date(o.max_time));
    send(')</li>');
    send('</ul>');
}

function fin_date(a) {
    "use strict";
    var d = new Date(a*1000);

    function pad(number) {  
        var r = String(number);  
        if ( r.length === 1 ) {  
            r = '0' + r;  
        }  
        return r;  
    }  
    return d.getDate()+'.'+(d.getMonth()+1)+'.'+d.getFullYear()+
	' klo '+pad(d.getHours())+'.'+pad(d.getMinutes());
}

function(head, req) {
    "use strict";
    var row;
    start({
	"headers": {
	    "Content-Type": "text/html; charset=UTF-8"
	}
    });
    send('<h1>Lämpötilat Ahmalla</h1>');

    while(row = getRow()) {
	format(row.value.out,"Ulkona");
	format(row.value.in,"Sisällä");
	format(row.value.box,"Laitetila");
    }
}
