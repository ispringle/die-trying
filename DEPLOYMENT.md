# Deployment

Docker builds it, Cloudflare Pages hosts it.

## Option 1: GitHub Actions

Set these secrets in your GitHub repo:
- `CLOUDFLARE_API_TOKEN`
- `CLOUDFLARE_ACCOUNT_ID`

Create a Pages project in Cloudflare named `die-trying` (or update `.github/workflows/deploy.yml` if you name it something else).

Push to master. It deploys automatically.

## Option 2: Manual Deploy

```bash
# Build and extract
docker build -t die-trying-builder .
docker create --name builder die-trying-builder
docker cp builder:/app/out ./out
docker rm builder

# Deploy with Wrangler
wrangler pages deploy out --project-name=die-trying
```

## Test Locally First

```bash
docker build -t die-trying-builder .
docker create --name builder die-trying-builder
docker cp builder:/app/out ./out
docker rm builder
python3 -m http.server 8000 -d out
```

Visit http://localhost:8000

## Required Files

- `Dockerfile`
- `build.lisp`
- `die-trying.asd`
- `main.lisp`
- `www/`

## Troubleshooting

**Build fails:** Read the error. Check your files aren't broken.

**Deploy fails:** Check your secrets. Check the project name matches. Check the Actions logs.

**Looks wrong:** Hard refresh. Cloudflare caches everything.

**Works locally, not in CI:** Make sure everything's committed.

## Notes

This is absurdly over-engineered but it works. The whole build takes ~20-30 seconds in CI.
