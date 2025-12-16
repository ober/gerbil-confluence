# Gerbil Confluence Client

A comprehensive Confluence REST API client written in Gerbil Scheme, supporting all major Confluence Cloud REST API v1 endpoints.

## Features

- **Complete API Coverage**: Support for 60+ Confluence REST API endpoints
- **Content Management**: Create, read, update, and delete pages and blog posts
- **Space Management**: List, create, update, and delete spaces
- **Collaboration**: Comments, labels, and attachments
- **User & Group Management**: Query users and groups
- **Advanced Features**: Content properties, restrictions, versioning, and audit logs
- **Secure Authentication**: Encrypted password storage using AES-256-CTR
- **CLI Interface**: Easy-to-use command-line interface for all operations

## Installation

### Build from Source

Prerequisites:
- Gerbil Scheme installed at `/opt/gerbil`
- Docker (for static builds)

```bash
make build
```

For static Linux builds:

```bash
make linux-static-docker
```

Install the binary:

```bash
make install
```

## Configuration

Create a configuration file at `~/.confluence.yaml`:

```yaml
url: https://your-instance.atlassian.net
user: your-email@example.com
space: YOUR-SPACE-KEY
```

Setup encrypted authentication:

```bash
confluence config
```

This will prompt for your password and generate an encrypted `secrets` entry to add to your config file.

## Usage

### Content Operations

#### Create a Page

```bash
# Create a page from a file (file name becomes page title with hyphens replaced by spaces)
confluence create my-new-page.cml
```

#### Update a Page

```bash
# Update using just content ID (fetches current version automatically)
confluence update 123456 updated-content.cml

# Update with explicit version
confluence update-batch 5 123456 "Page Title" content.cml
```

#### Get Content

```bash
# Get content metadata
confluence get 123456

# Get content body
confluence body 123456

# Get content info (including children)
confluence info 123456
```

#### Delete Content

```bash
confluence remove-doc 123456
```

#### Search Content

```bash
# Simple text search
confluence search "search term"

# CQL search
confluence search "type=page AND space=DEV"
```

### Space Operations

#### List Spaces

```bash
confluence list-spaces
```

#### Get Space Information

```bash
confluence get-space SPACEKEY
```

#### Create a Space

```bash
confluence create-space MYKEY "My Space Name" "Space description"
```

#### Delete a Space

```bash
confluence delete-space SPACEKEY
```

### Label Operations

#### Get Labels

```bash
confluence get-labels 123456
```

#### Add Labels

```bash
# Add multiple labels (comma-separated)
confluence add-labels 123456 "label1,label2,label3"
```

#### Remove a Label

```bash
confluence remove-label 123456 labelname
```

### Comment Operations

#### Get Comments

```bash
confluence get-comments 123456
```

#### Create a Comment

```bash
confluence create-comment 123456 "This is my comment text"
```

### Attachment Operations

#### List Attachments

```bash
confluence get-attachments 123456
```

### Content Hierarchy

#### Get Children

```bash
# Get immediate children of a page
confluence get-children 123456
```

#### Get Descendants

```bash
# Get all descendants at any level
confluence get-descendants 123456
```

### User & Group Operations

#### Get Current User

```bash
confluence get-current-user
```

#### Get User Information

```bash
confluence get-user john.doe
```

#### List Groups

```bash
confluence list-groups
```

#### Get Group Information

```bash
confluence get-group developers
```

### Version History

#### Get Content History

```bash
confluence get-history 123456
```

#### List All Versions

```bash
confluence list-versions 123456
```

### System Information

```bash
confluence system-info
```

### Content Format Conversion

#### Convert Wiki Markup to Storage Format

```bash
confluence convert my-page.cmd
```

#### Generic Format Converter

```bash
# Convert from wiki to storage format
confluence converter input.txt wiki storage

# Available formats: wiki, storage, view, editor, exported_view, styled_view
```

#### Markdown to Confluence

```bash
confluence md2c my-document.md
```

## API Reference

### Content API

- `create` - Create new content
- `update` / `update-batch` - Update existing content
- `get` - Get content by ID
- `body` - Get content body
- `search` - Search content using CQL
- `remove-doc` - Delete content
- `get-children` - Get child pages
- `get-descendants` - Get all descendant pages
- `get-history` - Get content history
- `list-versions` - List all content versions

### Space API

- `list-spaces` - List all spaces
- `get-space` - Get space information
- `create-space` - Create a new space
- `update-space` - Update a space
- `delete-space` - Delete a space
- Space properties: `get-space-properties`, `get-space-property`, `create-space-property`, `update-space-property`, `delete-space-property`
- Space settings: `get-space-settings`, `update-space-settings`

### Label API

- `get-labels` - Get labels for content
- `add-labels` - Add labels to content
- `remove-label` - Remove a label from content

### Attachment API

- `get-attachments` - List attachments for content
- `get-attachment` - Get specific attachment

### Comment API

- `get-comments` - Get comments for content
- `create-comment` - Create a comment

### Content Properties API

- `get-content-properties` - List content properties
- `get-content-property` - Get specific property
- `create-content-property` - Create property
- `update-content-property` - Update property
- `delete-content-property` - Delete property

### Restrictions API

- `get-content-restrictions` - Get all restrictions
- `get-restrictions-by-operation` - Get restrictions for specific operation

### User API

- `get-current-user` - Get current user information
- `get-user` - Get user by username/key/account ID
- `get-user-groups` - Get groups for a user

### Group API

- `list-groups` - List all groups
- `get-group` - Get group information
- `get-group-members` - Get group members

### Search API

- `search` - Text or CQL search
- `search-by-cql` - Advanced CQL search
- `general-search` - General search across all types

### Template API

- `get-content-templates` - Get content templates
- `get-blueprint-templates` - Get blueprint templates
- `get-template` - Get specific template

### System API

- `system-info` - Get system information

### Audit API

- `get-audit-records` - Get audit log records

## Programmatic API

All CLI commands are available as Gerbil functions. Import the client module:

```scheme
(import :ober/confluence/client)

;; List spaces
(list-spaces)

;; Get a space
(get-space "MYSPACE")

;; Create content
(create "my-page.cml")

;; Search with CQL
(search "type=page AND label=documentation")

;; Get labels
(get-labels "123456")

;; Add labels
(add-labels "123456" '("important" "reviewed"))
```

### Available Functions

#### Space Functions
- `(list-spaces #!key (limit 25) (start 0) (expand []))`
- `(get-space space-key #!key (expand []))`
- `(create-space key name description #!key (private #f))`
- `(update-space space-key name description #!key (homepage-id #f))`
- `(delete-space space-key)`
- `(get-space-content space-key #!key (type "page") (limit 25) (start 0) (expand []))`
- `(get-space-properties space-key #!key (limit 10) (start 0) (expand []))`
- `(get-space-property space-key key)`
- `(create-space-property space-key key value)`
- `(update-space-property space-key key value)`
- `(delete-space-property space-key key)`
- `(get-space-settings space-key)`
- `(update-space-settings space-key settings)`

#### Content Functions
- `(get id)`
- `(get-more id)`
- `(create content-file)`
- `(update id content-file)`
- `(update-batch version id title content-file)`
- `(remove-doc id)`
- `(search query)`
- `(body id)`

#### Content Hierarchy Functions
- `(get-content-children id #!key (type "page") (limit 25) (start 0) (expand []))`
- `(get-content-descendants id #!key (type "page") (limit 25) (start 0) (expand []))`

#### Label Functions
- `(get-labels id #!key (limit 200) (start 0))`
- `(add-labels id labels)` - labels is a list of label names
- `(remove-label id label)`

#### Attachment Functions
- `(get-attachments id #!key (limit 50) (start 0) (expand []))`
- `(get-attachment id attachment-id #!key (expand []))`

#### Comment Functions
- `(get-comments id #!key (limit 25) (start 0) (expand []))`
- `(create-comment parent-id comment-text)`

#### Property Functions
- `(get-content-properties id #!key (limit 10) (start 0) (expand []))`
- `(get-content-property id key)`
- `(create-content-property id key value)`
- `(update-content-property id key value)`
- `(delete-content-property id key)`

#### User Functions
- `(get-current-user #!key (expand []))`
- `(get-user #!key (username #f) (key #f) (account-id #f) (expand []))`
- `(get-user-groups #!key (username #f) (key #f) (account-id #f) (limit 200) (start 0))`

#### Group Functions
- `(list-groups #!key (limit 200) (start 0))`
- `(get-group group-name #!key (expand []))`
- `(get-group-members group-name #!key (limit 200) (start 0) (expand []))`

#### Restriction Functions
- `(get-content-restrictions id #!key (expand []))`
- `(get-restrictions-by-operation id operation #!key (expand []))`

#### Version/History Functions
- `(get-content-history id #!key (expand []))`
- `(get-content-version id version-number #!key (expand []))`
- `(list-content-versions id #!key (limit 200) (start 0))`

#### Audit Functions
- `(get-audit-records #!key (start-date #f) (end-date #f) (search-string #f) (limit 1000) (start 0))`

#### System Functions
- `(get-system-info)`

#### Template Functions
- `(get-content-templates #!key (space-key #f) (limit 25) (start 0) (expand []))`
- `(get-blueprint-templates #!key (space-key #f) (limit 25) (start 0) (expand []))`
- `(get-template template-id)`

#### Search Functions
- `(search query)` - Simple text search or CQL search
- `(search-by-cql cql #!key (cql-context #f) (limit 25) (start 0) (expand []))`
- `(general-search query #!key (limit 20))`

#### Content Listing Functions
- `(list-content #!key (type "page") (space-key #f) (title #f) (status "current") (posting-day #f) (limit 25) (start 0) (expand []) (order-by #f))`

#### Format Conversion Functions
- `(convert markdown-file)`
- `(converter in-file in-format out-format)`
- `(md2c markdown-file)`

## API Version

This client uses Confluence Cloud REST API v1. The API version is documented in the OpenAPI specification at `docs/swagger.v3.json`.

## Expand Parameters

Many API endpoints support an `expand` parameter to include additional data in responses. Common expand values include:

- `space` - Include space information
- `version` - Include version information
- `history` - Include history information
- `ancestors` - Include ancestor pages
- `body.storage` - Include content body in storage format
- `body.view` - Include content body in view format
- `children.page` - Include child pages
- `children.comment` - Include comments
- `children.attachment` - Include attachments
- `metadata.labels` - Include labels
- `metadata.properties` - Include content properties
- `restrictions.read.restrictions.user` - Include read restrictions
- `restrictions.update.restrictions.user` - Include update restrictions

Example:

```scheme
(get-space "MYSPACE" expand: '("description.view" "homepage"))
```

## Development

### Project Structure

```
.
├── confluence/
│   ├── client.ss        # API client implementation
│   └── confluence.ss    # CLI interface
├── docs/
│   ├── swagger.v3.json  # OpenAPI specification
│   └── Swagger.org      # API documentation
├── Makefile            # Build configuration
├── gerbil.pkg          # Package configuration
└── README.md           # This file
```

### Building

```bash
# Install dependencies
make deps

# Build the project
make build

# Clean build artifacts
make clean
```

### Testing

Currently, there are no automated tests. Manual testing can be done using the CLI commands against a Confluence instance.

## Contributing

Contributions are welcome! Please ensure:

1. Code follows existing style conventions
2. New API endpoints match the Confluence REST API specification
3. CLI commands are added for new functionality
4. Documentation is updated

## License

© ober 2021-2024

## Related Links

- [Confluence Cloud REST API Documentation](https://developer.atlassian.com/cloud/confluence/rest/v1/intro/)
- [Gerbil Scheme](https://cons.io/)
- [CQL (Confluence Query Language)](https://developer.atlassian.com/cloud/confluence/advanced-searching-using-cql/)
