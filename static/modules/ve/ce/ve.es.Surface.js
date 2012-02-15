/**
 * Creates an ve.es.Surface object.
 * 
 * @class
 * @constructor
 * @param {jQuery} $container DOM Container to render surface into
 * @param {ve.dm.Surface} model Surface model to view
 */
ve.es.Surface = function( $container, model ) {
	// Inheritance
	ve.EventEmitter.call( this );

	// References for use in closures
	var	_this = this,
		$document = $( document ),
		$window = $( window );
	
	// Properties
	this.model = model;
	this.documentView = new ve.es.DocumentNode( this.model.getDocument(), this );
	this.contextView = null;
	this.clipboard = {};
	this.$ = $container
		.addClass( 'es-surfaceView' )
		.append( this.documentView.$ );
	this.emitUpdateTimeout = undefined;
	this.node = null;

	// Events
	this.documentView.$.bind( {
		'focus': function( e ) {
			$document.unbind( '.ce-surfaceView' );
			$document.bind( {
				'keydown.ce-surfaceView': function( e ) {
					return _this.onKeyDown( e );
				},
			} );
			_this.setNode();
		},
		'blur': function( e ) {
			$document.unbind( '.ce-surfaceView' );
		}
	} );

	this.$.mousedown( function(e) {
		return _this.onMouseDown( e );
	} );

	/*
	this.model.getDocument().on( 'update', function() {
		_this.emitUpdate( 25 );
	} );

	this.$.mousedown( function( e ) {
		return _this.onMouseDown( e );
	} );

	this.$.mouseup( function( e ) {
		//var selection = _this.getSelection();
		//_this.showFakeCursorAt(selection.start);
	} );

	this.$.on('paste', function( e ) {
		_this.onPaste( e );
	} );

	this.$.on('cut copy', function( e ) {
		_this.onCutCopy( e );
	} );
	*/

	// Initialization
	this.documentView.renderContent();
};

/* Methods */

ve.es.Surface.prototype.attachContextView = function( contextView ) {
	this.contextView = contextView;
};

ve.es.Surface.prototype.getModel = function() {
	return this.model;
};

ve.es.Surface.prototype.onKeyDown = function( e ) {
	switch ( e.keyCode ) {
		// Page up
		case 33:
		// Page down
		case 34:
		// Home
		case 36:
		// End
		case 35:
		// Up arrow
		case 38:
		// Down arrow
		case 40:
			this.setNode();
			break;
		// Left arrow
		case 37:
			this.setNode( 'left' );
			break;
		// Right arrow
		case 39:
			this.setNode( 'right' );
			break;
	}
};

ve.es.Surface.prototype.onMouseDown = function( e ) {
	this.setNode();
};

ve.es.Surface.prototype.setNode = function( direction ) {
	var _this = this;
	
	setTimeout( function() {
		var rangySelection = rangy.getSelection();

		if ( rangySelection.anchorNode === _this.node ) {
			return;
		}

		if ( rangySelection.anchorNode.nodeType !== 3 ) {
			if ( _this.node === null ) {
				throw "Value of this.node shouldn't be a null";
			}
			if ( direction !== 'left' && direction !== 'right' ) {
				throw "At this point value of direction should be 'left' or 'right'";
			}
			var	oldOffset = _this.getOffset( _this.node, 0 ),
				newOffset;

			if ( direction === 'left' ) {
				newOffset = _this.documentView.model.getRelativeContentOffset( oldOffset, -1 );
			} else if ( direction === 'right' ) {
				newOffset = _this.documentView.model.getRelativeContentOffset( oldOffset + _this.node.length, 1 );
			}
			_this.showCursorAt( newOffset );
			_this.setNode();
			return;
		}

		_this.node = rangySelection.anchorNode;
		
		console.log(_this.node); 
	}, 0 );
};

ve.es.Surface.prototype.getSelection = function() {
	var selection = rangy.getSelection();

	if ( selection.anchorNode === selection.focusNode && selection.anchorOffset === selection.focusOffset ) {
		// cursor
		var offset = this.getOffset( selection.anchorNode, selection.anchorOffset );
		return new ve.Range( offset, offset );
	} else {
		// selection		
		var offset1 = this.getOffset( selection.anchorNode, selection.anchorOffset );
		var offset2 = this.getOffset( selection.focusNode, selection.focusOffset );
		return new ve.Range( offset1, offset2 );
	}
};

ve.es.Surface.prototype.getOffset = function( localNode, localOffset ) {
	var $node = $( localNode );
	
	if ( $node.hasClass( 'ce-leafNode' ) ) {
		return this.documentView.getOffsetFromNode( $node.data('view') ) + 1;
	}
	
	while( !$node.hasClass( 'ce-leafNode' ) ) {
		$node = $node.parent();
	}
	
	var current = [$node.contents(), 0];
	var stack = [current];
	
	var offset = 0;
	
	while ( stack.length > 0 ) {
		if ( current[1] >= current[0].length ) {
			stack.pop();
			current = stack[ stack.length - 1 ];
			continue;
		}
		var item = current[0][current[1]];
		var $item = current[0].eq( current[1] );
		
		if ( item.nodeType === 3 ) {
			if ( item === localNode ) {
				offset += localOffset;
				break;
			} else {
				offset += item.textContent.length;
			}
		} else if ( item.nodeType === 1 ) {
			if ( $( item ).attr('contentEditable') === "false" ) {
				offset += 1;
			} else {
				if ( item === localNode ) {
					offset += localOffset;
					break;
				}
			
				stack.push( [$item.contents(), 0] );
				current[1]++;
				current = stack[stack.length-1];
				continue;
			}
		}
		current[1]++;
	}

	return this.documentView.getOffsetFromNode( $node.data('view') ) + 1 + offset;
};

ve.es.Surface.prototype.showCursorAt = function( offset ) {
	var $node = this.documentView.getNodeFromOffset( offset ).$;
	var current = [$node.contents(), 0];
	var stack = [current];
	var node;
	var localOffset;
	
	var index = 1 + this.documentView.getOffsetFromNode( $node.data('view') );
		
	while ( stack.length > 0 ) {
		if ( current[1] >= current[0].length ) {
			stack.pop();
			current = stack[ stack.length - 1 ];
			continue;
		}
		var item = current[0][current[1]];
		var $item = current[0].eq( current[1] );
		
		if ( item.nodeType === 3 ) {
			var length = item.textContent.length;
			if ( offset >= index && offset <= index + length ) {
				node = item;
				localOffset = offset - index;
				break;
			} else {
				index += length;
			}
		} else if ( item.nodeType === 1 ) {
			if ( $( item ).attr('contentEditable') === "false" ) {
				index += 1;
			} else {
				stack.push( [$item.contents(), 0] );
				current[1]++;
				current = stack[stack.length-1];
				continue;
			}
		}
		current[1]++;
	}
	var range = document.createRange();
	range.collapsed = true;
	range.setStart(node, localOffset);

	var sel = window.getSelection();
	sel.removeAllRanges();
	sel.addRange(range);
};

/*
ve.es.Surface.prototype.onCutCopy = function( e ) {
	var _this = this,
		key = rangy.getSelection().getRangeAt(0).toString().replace(/( |\r\n|\n|\r|\t)/gm,"");

	_this.clipboard[key] = ve.copyArray( _this.documentView.model.getData( _this.getSelection() ) );

	if ( event.type == 'cut' ) {
		setTimeout( function() {
			document.execCommand('undo', false, false);
		
			var selection = _this.getSelection();
			var tx = _this.model.getDocument().prepareRemoval( selection );
			_this.model.transact( tx );
			_this.showCursorAt( selection.start );
		}, 1 );
	}
};

ve.es.Surface.prototype.onPaste = function( e ) {
	var _this = this,
		insertionPoint = _this.getSelection().start;
	
	$('#paste').html('').show().css('top', $(window).scrollTop()).css('left', $(window).scrollLeft()).focus();
	
	setTimeout( function() {
		var key = $('#paste').hide().text().replace(/( |\r\n|\n|\r|\t)/gm,"");

		if ( _this.clipboard[key] ) {
			var tx = _this.documentView.model.prepareInsertion( insertionPoint, _this.clipboard[key]);
			_this.model.transact( tx );
			_this.showCursorAt(insertionPoint + _this.clipboard[key].length);
		} else {
			alert('i can only handle copy/paste from hybrid surface. sorry. :(');
		}
		
	}, 1 );
};
		
ve.es.Surface.prototype.onMouseDown = function( e ) {
	if ( this.worker !== null ) {
		clearInterval( this.worker );
	}

	var _this = this;
	
	setTimeout( function() {
		_this.node = rangy.getSelection().anchorNode;
		var prevText = _this.node.textContent;
		_this.worker = setInterval( function() {

			if ( ( _this.node.previousSibling !== null && _this.node.previousSibling.nodeType === 3 ) || ( _this.node.nextSibling !== null && _this.node.nextSibling.nodeType === 3 ) ) {
				console.log("!");
				var start = _this.getSelection().start;
				_this.node.parentNode.normalize();
				_this.showCursorAt( start );
				_this.node = rangy.getSelection().anchorNode;
			}
			
			
			var text = _this.node.textContent;

			if ( text === prevText ) {
				return;
			}
			
			var nodeOffset = _this.getOffset( _this.node, 0 );

            var sameFromLeft = 0,
                sameFromRight = 0,
                l = prevText.length;

            while ( sameFromLeft < l && prevText[sameFromLeft] == text[sameFromLeft] ) {
                ++sameFromLeft;
			}
			if ( prevText.length > sameFromLeft ) {
				l = l - sameFromLeft;
	            while ( sameFromRight < l && prevText[prevText.length - 1 - sameFromRight] == text[text.length - 1 - sameFromRight] ) {
	                ++sameFromRight;
				}
			}
			
            if ( sameFromLeft + sameFromRight !== prevText.length ) {
            	// delete
            	var range = new ve.Range( nodeOffset + sameFromLeft, nodeOffset + prevText.length - sameFromRight );
            	var tx = _this.model.getDocument().prepareRemoval( range );
            	_this.model.transact( tx );
            }
            
            if ( sameFromLeft + sameFromRight !== text.length ) {
				// insert
            	var data = text.split('').slice(sameFromLeft, text.length - sameFromRight);
            	var annotations = _this.model.getDocument().getAnnotationsFromOffset( nodeOffset + sameFromLeft - 1 );
            	ve.dm.DocumentNode.addAnnotationsToData( data, annotations );
            	var tx = _this.documentView.model.prepareInsertion( nodeOffset + sameFromLeft, data);
            	_this.model.transact( tx );
            }			

			prevText = text;
		}, 50 );
	}, 1 );
};

ve.es.Surface.prototype.emitUpdate = function( delay ) {
	if ( delay ) {
		if ( this.emitUpdateTimeout !== undefined ) {
			return;
		}
		var _this = this;
		this.emitUpdateTimeout = setTimeout( function() {
			_this.emit( 'update' );	
			_this.emitUpdateTimeout = undefined;
		}, delay );
	} else {
		this.emit( 'update' );	
	}
};

ve.es.Surface.prototype.showCursorAt = function( offset ) {
	var $node = this.documentView.getNodeFromOffset( offset ).$;
	var current = [$node.contents(), 0];
	var stack = [current];
	var node;
	var localOffset;
	
	var index = 1 + this.documentView.getOffsetFromNode( $node.data('view') );
		
	while ( stack.length > 0 ) {
		if ( current[1] >= current[0].length ) {
			stack.pop();
			current = stack[ stack.length - 1 ];
			continue;
		}
		var item = current[0][current[1]];
		var $item = current[0].eq( current[1] );
		
		if ( item.nodeType === 3 ) {
			var length = item.textContent.length;
			if ( offset >= index && offset <= index + length ) {
				node = item;
				localOffset = offset - index;
				break;
			} else {
				index += length;
			}
		} else if ( item.nodeType === 1 ) {
			if ( $( item ).attr('contentEditable') === "false" ) {
				index += 1;
			} else {
				stack.push( [$item.contents(), 0] );
				current[1]++;
				current = stack[stack.length-1];
				continue;
			}
		}
		current[1]++;
	}
	var range = document.createRange();
	range.collapsed = true;
	range.setStart(node, localOffset);

	var sel = window.getSelection();
	sel.removeAllRanges();
	sel.addRange(range);
};

ve.es.Surface.prototype.showFakeCursorAt = function( offset ) {
	var _this = this;

	var $node = _this.documentView.getNodeFromOffset( offset ).$;
	var current = [$node.contents(), 0];
	var stack = [current];
	var node;
	var localOffset;
	
	var index = 1 + _this.documentView.getOffsetFromNode( $node.data('view') );

	while ( stack.length > 0 ) {
		if ( current[1] >= current[0].length ) {
			stack.pop();
			current = stack[ stack.length - 1 ];
			continue;
		}
		var item = current[0][current[1]];
		var $item = current[0].eq( current[1] );
		
		if ( item.nodeType === 3 ) {
			var length = item.textContent.length;
			if ( offset >= index && offset <= index + length ) {
				node = item;
				localOffset = offset - index;
				break;
			} else {
				index += length;
			}
		} else if ( item.nodeType === 1 ) {
			if ( $( item ).attr('contentEditable') === "false" ) {
				index += 1;
			} else {
				stack.push( [$item.contents(), 0] );
				current[1]++;
				current = stack[stack.length-1];
				continue;
			}
		}
		current[1]++;
	}

	// Delay by 3 seconds - just for demo
	setTimeout(function() {
		var sel = rangy.getSelection();
		var range1 = sel.getRangeAt(0);			
		var range2 = rangy.createRange();
		
		range2.setStart(node, localOffset);
	
		sel.setSingleRange(range2);
		
		var position = rangy.getSelection().getStartDocumentPos();
		$('#fake-cursor').css('top', position.y).css('left', position.x);
		
		sel.setSingleRange(range1);
	}, 3000);
}
*/

/* Inheritance */

ve.extendClass( ve.es.Surface, ve.EventEmitter );
