now = new Date();
now_unix = Math.floor(now.getTime() / 1000);

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

function link_range(title,start,end) {
    var r;
    if (start === null && end == null) r = '';
    else if (start === null) r = 'endkey='+end;
    else if (end === null) r = 'startkey='+start;
    else r = 'startkey='+start+'&endkey='+end;

    send('<li><a href="?');
    send(r);
    send('">');
    send(title);
    send('</li>');
}

function day_ago() {
    return now_unix - 86400;
}

function year_ago() {
    var old = new Date(now.getTime());
    // Leap year aware on the contrary to unix timestamp trick
    old.setFullYear(old.getFullYear()-1);
    return Math.floor(old.getTime() / 1000);
}

function month_ago() {
    var old = new Date(now.getTime());
    old.setMonth(old.getMonth()-1);
    return Math.floor(old.getTime() / 1000);
}

function year_start() {
    var old = new Date(now.getFullYear(),0,1);
    return Math.floor(old.getTime() / 1000);
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

    if (row == undefined) {
	send('<p>Ei dataa tällä aikavälillä</p>');
    } else {
	send('<p>Aikavälillä ');
	send(fin_date(callAll(row,"start_time",Math.min)));
	send(' – ');
	send(fin_date(callAll(row,"end_time",Math.max)));
	send('</p>');
	
	format(row.value.out,"Ulkona");
	format(row.value.in,"Sisällä");
	format(row.value.box,"Laitetila");
    }	

    send('<h2>Vaihda aikaväliä</h2><ul>');
    link_range('Kaikki mittaukset',null,null);
    link_range('Viimeisin vuorokausi',day_ago(),now_unix);
    link_range('Viimeisin kuukausi',month_ago(),now_unix);
    link_range('Vuoden alusta',year_start(),null);
    link_range('Viimeisin vuosi',year_ago(),now_unix);
    link_range('Tästä hetkestä lähtien',now_unix,null);

    send('</ul>');

    send('<p><a title="Tutki sovelluksen lähdekoodia" '+
	 'href="https://github.com/zouppen/ahma_stats">'+
	 'Lähdekoodi</a> löytyy GitHubista.</p>');
}
