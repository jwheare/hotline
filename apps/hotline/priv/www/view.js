var VIEW = {};
(function () {
    // Helpers
    function escapeText (text) {
        return text
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;')
            .replace(/'/g, '&apos;');
    }
    function autolink (text) {
        return linkify(text, {
            callback: function (cbText, href) {
                var html = '';
                if (href) {
                    html += _.template('<a href="<%= href %>" target="_blank">', {href : href});
                }
                html += escapeText(cbText);
                if (href) {
                    html += '</a>';
                }
                return html;
            }
        });
    }
    
    // News
    VIEW.NewsView = Backbone.View.extend({
        el: 'div#news',
        
        initialize: function () {
            this.model.bind("change", $.proxy(this, "render"));
        },
        
        render: function () {
            $(this.el).html(autolink(this.messages()));
        },
        
        messages: function () {
            return this.model.get('messages').replace(/\r\n/g, '\n').replace(/\r/g, '\n');
        }
    });
    
    // Users
    var UserView = Backbone.View.extend({
        tagName: 'li',
        
        render: function () {
            return this.renderNick().renderIcon();
        },
        
        nick: function () {
            return this.model.get('nick');
        },
        icon: function () {
            return '/icons/' + this.model.get('icon') + '.gif';
        },
        
        renderNick: function () {
            $(this.el).text(this.nick());
            return this;
        },
        renderIcon: function () {
            $(this.el).css('background-image', 'url(' + this.icon() + ')');
            return this;
        }
    });
    
    VIEW.UserListView = Backbone.View.extend({
        el: 'ul#members',
        
        initialize: function () {
            this.collection.bind("reset", $.proxy(this, "render"));
        },
        
        render: function () {
            $(this.el).empty();
            this.collection.each($.proxy(this, "renderUser"));
            return this;
        },
        
        renderUser: function (user) {
            var userView = new UserView({
                model: user,
                className: 'status_' + user.get('status')
            });
            $(this.el).append(userView.render().el);
        }
    });
    
    // Messages
    
    var MessageView = Backbone.View.extend({
        render: function () {
        
        },
        
        getMember: function () {
            return memberList.get(this.model.get('from_id'));
        }
    });
    
    VIEW.ConversationsView = Backbone.View.extend({
        initialize: function () {
            this.collection.bind("add", $.proxy(this, "renderMessage"));
        },
        
        renderMessage: function (message) {
            var messageView = new MessageView({
                model: message
            });
            messageView.render();
        }
    });
    
    // Lines
    
    var LineView = Backbone.View.extend({
        tagName: 'div',
        className: 'row',
        
        initialize: function () {
            this.model.bind("hide", $.proxy(this, 'hide'));
        },
        
        hide: function () {
            $(this.el).hide();
            return this;
        },
        
        render: function () {
            $(this.el).addClass('type_' + this.type());
            var date = this.date();
            var timestamp = $('<span>')
                .attr('title', date.toString('dddd, MMMM dd, yyyy HH:mm:ss'))
                .addClass('timestamp')
                .text(date.toString('HH:mm'));
            $(this.el)
                .html(this.renderer())
                .prepend('<span class="g">] </span>')
                .prepend(timestamp)
                .prepend(' <span class="g">[</span>');
            
            return this;
        },
        
        type: function () {
            return this.model.get('type');
        },
        date: function () {
            return new Date(this.model.get('time') * 1000);
        },
        
        renderer: function () {
            // defined in sub classes
        }
    }, {
        makeRenderer: function (renderer) {
            return LineView.extend({ renderer: renderer });
        }
    });
    
    var LineTypes = {
        chat_msg: LineView.makeRenderer(function () {
            return autolink(this.model.get('msg').replace(/^\r/, ''));
        }),
        server_msg: LineView.makeRenderer(function () {
            return '[' + escapeText(this.model.get('from')) + '] ' + autolink(this.model.get('msg'));
        }),
        socket_closed: LineView.makeRenderer(function () {
            return 'Disconnected';
        }),
        user_joined: LineView.makeRenderer(function () {
            return '→ ' + escapeText(this.model.get('user').nick) + ' joined';
        }),
        user_left: LineView.makeRenderer(function () {
            return '← ' + escapeText(this.model.get('user').nick) + ' left';
        }),
        user_nick_change: LineView.makeRenderer(function () {
            return escapeText(this.model.get('old_nick')) + ' → ' + escapeText(this.model.get('user').nick);
        })
    };
    
    var ComboLineTypes = {
        user_nick_change: function (line, lastLine) {
            // Check predicates
            if (lastLine.get('type') != 'user_joined') {
                return false;
            }
            if (line.get('user').id != lastLine.get('user').id) {
                return false;
            }
            // Return a renderer if the predicates passed
            return LineView.makeRenderer(function () {
                lastLine.trigger('hide');
                return '→ ' + escapeText(this.model.get('old_nick')) + ' → ' + escapeText(this.model.get('user').nick) + ' joined';
            });
        }
    };
    
    VIEW.LinesView = Backbone.View.extend({
        el: 'div#lines',
        
        initialize: function () {
            this.collection.bind("add", $.proxy(this, "renderLine"));
        },
        
        renderLine: function (line) {
            var scrolledUp = this.scrolledUpDistance();
            var lastLine = this.collection.at(this.collection.indexOf(line) - 1);
            
            var type = line.get('type');
            
            var viewClass;
            if (lastLine && ComboLineTypes[type]) {
                viewClass = ComboLineTypes[type](line, lastLine);
            }
            viewClass = viewClass || LineTypes[type];
            var lineView = new viewClass({
                model: line
            });
            $(this.el).append(lineView.render().el);
            
            // Keep scrolled to bottom
            if (scrolledUp == 0) {
                this.scrollToBottom();
            }
        },
        
        height: function () {
            return $(this.el).height();
        },
        scrollHeight: function () {
            return $(this.el).prop('scrollHeight');
        },
        viewportTop: function () {
            return $(this.el).scrollTop();
        },
        viewportBottom: function () {
            return this.viewportTop() + this.height();
        },
        scrollTo: function (position) {
            $(this.el).scrollTop(position);
            return this;
        },
        scrolledUpDistance: function () {
            return this.scrollHeight() - this.viewportBottom();
        },
        scrollToBottom: function () {
            this.scrollTo(this.scrollHeight());
            return this;
        }
    });
})();
