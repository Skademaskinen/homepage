
function sleep(ms){
    return new Promise(resolve => setTimeout(resolve, ms))
}

function getCookie(cname) {
    let name = cname + "=";
    let decodedCookie = decodeURIComponent(document.cookie);
    let ca = decodedCookie.split(';');
    for(let i = 0; i <ca.length; i++) {
        let c = ca[i];
        while (c.charAt(0) == ' ') {
            c = c.substring(1);
        }
        if (c.indexOf(name) == 0) {
            return c.substring(name.length, c.length);
        }
    }
    return "";
}

function deleteCookie(name){
    document.cookie = name + "=" + getCookie(name)+";path=/;max-age=0;"
}

function setCookie(cookie) {
    document.cookie = cookie+";SameSite=None;Secure;"
}

function go_to(id){
    console.log(id)
    window.location.href = "https://"+id+"."+window.location.hostname
}