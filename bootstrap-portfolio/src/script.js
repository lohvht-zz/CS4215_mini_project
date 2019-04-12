
$(document).ready(function () {
    applyHeader(); // done
    applyNavigation(); // done
    applyMailTo(); // ignored
    applyResize();
    checkHash();
    applyFormChecking();
});

/* HEADER FUNCTIONS */
function applyHeader() {
    console.log("Here")
    // $('.jumbotron').css({ height: ($(window).height()) + 'px' });
}

/* NAVIGATION FUNCTIONS */
function applyNavigation() {
    applyClickEvent(); // done
    applyNavigationFixForPhone();
    applyScrollSpy(); // NOTE: ABANDONED
    applyStickyNavigation(); // TODO: Apply this to resize as well
}

function applyClickEvent() {
    $('a[href*="#"]').on('click', function (e) {
        e.preventDefault();

        if ($($.attr(this, 'href')).length > 0) {
            $('html, body').animate(
                {
                    scrollTop: $($.attr(this, 'href')).offset().top
                }, 400);
        }
        return false;
    });
}

function applyNavigationFixForPhone() {
    $('.navbar li a').click(function (event) {
        $('.navbar-collapse').removeClass('in').addClass('collapse');
    });
}

function applyScrollSpy() {
    $('#navigation-bar').on('activate.bs.scrollspy', function () {
        window.location.hash = $('.nav .active a').attr('href').replace('#', '#/');
    });
}

function applyStickyNavigation() {
    // i.e. the chevron button is 80px height with 20px offset,
    // attached fixed property adds the extra 60 height to nav for the allowance
    $(window).on('scroll', function () {
        stickyNavigation();
    });

    stickyNavigation();
}

function stickyNavigation() {
    if ($(window).scrollTop() > $('.scroll-down').offset().top + 20) {
        $('body').addClass('fixed');
    } else {
        $('body').removeClass('fixed');
    }
}

/* MAILTO FUNCTION */
function applyMailTo() {
    $('a[href*=mailto]').on('click', function (e) {
        var lstrEmail = $(this).attr('href').replace('mailto:', '');

        // forces lowercase
        lstrEmail = lstrEmail.split('').reverse().join('')
        $(this).attr('href', 'mailto:' + lstrEmail);
    });
}

/* RESIZE FUNCTION */
function applyResize() {
    applyResizeAllTextAreasByInput();
    $(window).on('resize', function () {
        stickyNavigation();
        // lnStickyNavigation = ;
        // resizes window when scrolling down such that the jumbotron's panel should
        // scale with the size of the window
        // $('.jumbotron').css({ height: ($(window).height()) + 'px' });
        resizeAllTextAreas();
    });
}

// Apply resizing to text area on change of input TODO: rename function to fit
// the input handler as well
function applyResizeAllTextAreasByInput() {
    var textAreas = document.getElementsByTagName('textarea');
    for(var i = 0; i < textAreas.length; i++){
        textAreas[i].setAttribute('style', 'height: auto;overflow-y:hidden;');
        
        textAreas[i].addEventListener("input", function () {
            // Listens to input and changes the height
            this.style.height = 'auto';
            if(this.scrollHeight > 0) {
                this.style.height = (this.scrollHeight) + 'px';
            }
            // Listens to input to output character limit TODO: implement some char limit
            var maxLength = this.getAttribute("maxlength");
            var currentLength = this.value.length;
        }, false);
    }
}

function resizeAllTextAreas() {
    console.log("this is resized: ", textAreas);
    var textAreas = document.getElementsByTagName('textarea');

    for (var i = 0; i < textAreas.length; i++) {
        textAreas[i].style.height = 'auto';
        if(textAreas[i].scrollHeight > 0) {
            textAreas[i].style.height = (textAreas[i].scrollHeight) + 'px';
        }
    }
}

/* HASH FUNCTION */
function checkHash() {
    lstrHash = window.location.hash.replace('#/', '#');
    if ($('a[href="' + lstrHash + '"]').length > 0) {
        $('a[href="' + lstrHash + '"]').trigger('click');
    }
}

/* form checking elements */
function applyFormChecking() {
    $('#contactForm').validate({
        submitHandler: function(form) {
            // FORM SUBMIT HANDLER, IMPLEMENT ME

            // clear the form and give an alert!
            var formElement = document.getElementById('contactForm');
            formElement.reset();
            alert("Your form has been submitted successfully!");
        },
        highlight: function(element, errorClass) {
            $(element).fadeOut(100, function(){
                $(element).fadeIn(100);
            });
        },
    });
}
