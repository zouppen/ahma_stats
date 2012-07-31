function(newDoc, oldDoc, userCtx) {
    function inGroup(name) {
	return userCtx.roles.indexOf('_admin') !== -1;
    }

    if (!(inGroup('_admin') || inGroup('ahma'))) {
	throw({forbidden: 'Only members of Ahma may edit the database'});
    }
}
