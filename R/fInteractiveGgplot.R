#' Rend un ggplot interactif et personnalisable (HTML)
#'
#' @param Plot          Un objet ggplot.
#' @param tooltip       Esthétiques affichées au survol. NULL = automatique :
#'                      "text" si une esthétique text existe, sinon "all".
#' @param couleurs      TRUE pour afficher un sélecteur de couleur par groupe.
#' @param editable      TRUE pour pouvoir modifier titre, axes et légende au clic.
#' @param titre_panneau Texte au-dessus des sélecteurs de couleur.
#' @param nom_export    Nom du fichier image téléchargé via l'icône appareil photo.
#' @param format_export Format de l'image téléchargée : "png", "svg", "jpeg" ou "webp".
#' @param largeur,hauteur Taille du graphique en pixels (NULL = automatique).
#' @param fichier       Chemin d'un fichier .html à enregistrer (NULL = pas d'enregistrement).
#'
#' @return Un widget plotly.
#' @export
fInteractiveGGplot <- function(Plot,
                               tooltip       = NULL,
                               couleurs      = TRUE,
                               editable      = TRUE,
                               titre_panneau = "Couleurs :",
                               nom_export    = "graphique",
                               format_export = c("png", "svg", "jpeg", "webp"),
                               largeur       = NULL,
                               hauteur       = NULL,
                               fichier       = NULL) {

  format_export <- match.arg(format_export)

  # Infobulle automatique : "text" si le graphique en définit une
  if (is.null(tooltip)) {
    aes_plot   <- names(Plot$mapping)
    aes_layers <- unlist(lapply(Plot$layers, function(l) names(l$mapping)))
    tooltip    <- if ("text" %in% c(aes_plot, aes_layers)) "text" else "all"
  }

  p <- plotly::ggplotly(Plot, tooltip = tooltip, width = largeur, height = hauteur)

  p <- plotly::config(
    p,
    editable               = editable,
    locale                 = "fr",
    displaylogo            = FALSE,
    modeBarButtonsToRemove = c("lasso2d", "select2d"),
    toImageButtonOptions   = list(format = format_export,
                                  filename = nom_export,
                                  scale = 2)
  )

  js <- r"---(
function(el, x, opts) {
  if (!opts.couleurs && !opts.editable) return;

  function copie(o) { return JSON.parse(JSON.stringify(o)); }

  // Lit une couleur ('#rrggbb', 'rgb()', 'rgba()') -> {hex, alpha}
  function analyse(c) {
    if (typeof c !== 'string') return null;
    c = c.trim();
    if (c.charAt(0) === '#') {
      var h = c.length === 4 ? '#' + c[1] + c[1] + c[2] + c[2] + c[3] + c[3] : c.slice(0, 7);
      return { hex: h.toLowerCase(), alpha: 1 };
    }
    var m = c.match(/[\d.]+/g);
    if (!m || m.length < 3) return null;
    var hex = '#' + m.slice(0, 3).map(function(v) {
      return ('0' + Math.round(parseFloat(v)).toString(16)).slice(-2);
    }).join('');
    return { hex: hex, alpha: m.length > 3 ? parseFloat(m[3]) : 1 };
  }

  // Reconstruit une couleur en conservant la transparence d'origine
  function versCouleur(hex, alpha) {
    if (alpha >= 1) return hex;
    var r = parseInt(hex.slice(1, 3), 16),
        g = parseInt(hex.slice(3, 5), 16),
        b = parseInt(hex.slice(5, 7), 16);
    return 'rgba(' + r + ',' + g + ',' + b + ',' + alpha + ')';
  }

  function lire(tr, chemin) {
    return chemin.split('.').reduce(function(o, k) { return o ? o[k] : undefined; }, tr);
  }

  var CHEMINS = ['line.color', 'marker.color', 'marker.line.color', 'fillcolor'];

  // État d'origine pour la réinitialisation
  var dataInit   = copie(x.data);
  var layoutInit = copie(x.layout);
  var configInit = copie(x.config || {});

  // Regroupe les traces par groupe de légende
  var groupes = {};
  if (opts.couleurs) {
    el.data.forEach(function(tr, i) {
      var g = tr.legendgroup || tr.name;
      if (!g) return;
      (groupes[g] = groupes[g] || { traces: [] }).traces.push(i);
    });
  }

  Object.keys(groupes).forEach(function(g) {
    var G = groupes[g];

    // Couleur de base du groupe : la première couleur unie trouvée
    for (var t = 0; t < G.traces.length && !G.origine; t++) {
      var tr = el.data[G.traces[t]];
      for (var k = 0; k < CHEMINS.length; k++) {
        var a = analyse(lire(tr, CHEMINS[k]));
        if (a && a.alpha > 0) { G.origine = a.hex; break; }
      }
    }
    if (!G.origine) { delete groupes[g]; return; }

    // Seuls les attributs qui portent cette couleur seront modifiés
    // (un fond blanc de boxplot ou un contour transparent restent intacts)
    G.cibles = [];
    G.traces.forEach(function(i) {
      CHEMINS.forEach(function(ch) {
        var a = analyse(lire(el.data[i], ch));
        if (a && a.hex === G.origine) G.cibles.push({ i: i, chemin: ch, alpha: a.alpha });
      });
    });
    G.actuelle = G.origine;
  });

  function appliquer(g, hex) {
    var G = groupes[g], parTrace = {};
    G.cibles.forEach(function(c) {
      (parTrace[c.i] = parTrace[c.i] || {})[c.chemin] = versCouleur(hex, c.alpha);
    });
    Object.keys(parTrace).forEach(function(i) {
      Plotly.restyle(el, parTrace[i], [Number(i)]);
    });
    G.input.value = hex;
  }

  var historique = [];
  var noms = Object.keys(groupes);

  // Panneau
  var panneau = document.createElement('div');
  panneau.style.cssText = 'display:flex;flex-wrap:wrap;align-items:center;gap:12px;margin:8px 0;font:13px sans-serif';

  if (noms.length) {
    var titre = document.createElement('b');
    titre.style.width = '100%';
    titre.textContent = opts.titre;
    panneau.appendChild(titre);

    noms.forEach(function(g) {
      var G = groupes[g];
      var lab = document.createElement('label');
      lab.textContent = String(g).replace(/^\((.*),1\)$/, '$1') + ' ';
      var inp = document.createElement('input');
      inp.type = 'color';
      inp.value = G.origine;
      inp.oninput  = function() { appliquer(g, this.value); };
      inp.onchange = function() {
        historique.push({ g: g, couleur: G.actuelle });
        G.actuelle = this.value;
      };
      G.input = inp;
      lab.appendChild(inp);
      panneau.appendChild(lab);
    });
  }

  function bouton(texte, action) {
    var b = document.createElement('button');
    b.textContent = texte;
    b.style.cssText = 'padding:4px 10px;border:1px solid #bbb;border-radius:4px;background:#f7f7f7;cursor:pointer';
    b.onclick = action;
    return b;
  }

  var barre = document.createElement('div');
  barre.style.cssText = 'width:100%;display:flex;gap:8px;margin-top:4px';

  if (noms.length) {
    barre.appendChild(bouton('\u21B6 Annuler', function() {
      var h = historique.pop();
      if (!h) return;
      appliquer(h.g, h.couleur);
      groupes[h.g].actuelle = h.couleur;
    }));
  }

  barre.appendChild(bouton('\u27F2 R\u00e9initialiser', function() {
    historique = [];
    Plotly.react(el, copie(dataInit), copie(layoutInit), copie(configInit));
    noms.forEach(function(g) {
      groupes[g].actuelle = groupes[g].origine;
      groupes[g].input.value = groupes[g].origine;
    });
  }));

  panneau.appendChild(barre);
  el.parentNode.insertBefore(panneau, el);
}
)---"

p <- htmlwidgets::onRender(
  p, js,
  data = list(titre = titre_panneau, couleurs = couleurs, editable = editable)
)

if (!is.null(fichier)) {
  htmlwidgets::saveWidget(p, fichier, selfcontained = TRUE)
}

p
}
