function format(o,name) {
    "use strict";

    send('<h2>');
    send(name);
    send('</h2><ul>');

    send('<li>Viimeisin mittaus ');
    send(o.end_value.toFixed(1));
    send('°C (');
    send(fin_date(o.end_time));
    send(')</li>');

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

function callAll(row,key,f) {
    var a = row.value;
    return f(a.in[key],
	     a.out[key],
	     a.box[key]
	    );
}

function(head, req) {
    "use strict";
    start({
	"headers": {
	    "Content-Type": "text/html; charset=UTF-8"
	}
    });

    var row = getRow(); // Process only one, assume no grouping

    send('<h1>Lämpötilat Ahmalla</h1>');

    send('<p>Aikavälillä ');
    send(fin_date(callAll(row,"start_time",Math.min)));
    send(' – ');
    send(fin_date(callAll(row,"end_time",Math.max)));
    send('</p>');

    format(row.value.out,"Ulkona");
    format(row.value.in,"Sisällä");
    format(row.value.box,"Laitetila");
}
