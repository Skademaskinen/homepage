module P10.Autoscaler where
import Data.Aeson.QQ (aesonQQ)
import P10.Constants (aesonToKubeconfig, autoscalerPort, autoscalerExposedPort, forecasterPort, forecasterExposedPort, dbName, dbUser, dbPassword, dbHost, dbPort)

autoscaler :: String
autoscaler = aesonToKubeconfig [[aesonQQ|
    {
        apiVersion: "v1",
        kind: "ServiceAccount",
        metadata: {
            name: "autoscaler",
            namespace: "default"
        }
    }
|], [aesonQQ|
    {
        apiVersion: "rbac.authorization.k8s.io/v1",
        kind: "ClusterRole",
        metadata: {
            name: "autoscaler-role"
        },
        rules: [
            {
                apiGroups: ["apps"],
                resources: ["services", "deployments"],
                verbs: ["get", "list", "watch"]
            }
        ]
    }
|], [aesonQQ|
    {
        apiVersion: "rbac.authorization.k8s.io/v1",
        kind: "ClusterRoleBinding",
        metadata: {
            name: "autoscaler-rolebinding"
        },
        roleRef: {
            apiGroup: "rbac.authorization.k8s.io",
            kind: "ClusterRole",
            name: "autoscaler-role"
        },
        subjects: [
            {
                kind: "ServiceAccount",
                name: "autoscaler",
                namespace: "default"
            }
        ]
    }
|], [aesonQQ|
    {
        apiVersion: "v1",
        kind: "Service",
        metadata: {
            name: "autoscaler"
        },
        spec: {
            type: "NodePort",
            selector: {
                app: "autoscaler"
            },
            ports: [
                {
                    port: #{autoscalerPort},
                    targetPort: #{autoscalerPort},
                    nodePort: #{autoscalerExposedPort}
                }
            ]
        }
    }
|], [aesonQQ|
    {
        apiVersion: "v1",
        kind: "Service",
        metadata: {
            name: "forecaster"
        },
        spec: {
            type: "NodePort",
            selector: {
                app: "autoscaler"
            },
            ports: [
                {
                    port: #{forecasterPort},
                    targetPort: #{forecasterPort},
                    nodePort: #{forecasterExposedPort}
                }
            ]
        }
    }
|], [aesonQQ|
    {
        apiVersion: "apps/v1",
        kind: "Deployment",
        metadata: {
            annotations: {},
            name: "autoscaler",
            namespace: "default"
        },
        spec: {
            selector: {
                matchLabels: {
                    app: "autoscaler"
                }
            },
            template: {
                metadata: {
                    labels: {
                        app: "autoscaler"
                    }
                },
                spec: {
                    containers: [
                        {
                            env: [
                                {
                                    name: "Autoscaler__Apis__Forecaster__Url",
                                    value: #{"http://forecaster:" ++ show forecasterPort}
                                },
                                {
                                    name: "Autoscaler__Apis__Forecaster__Mock",
                                    value: "false"
                                },
                                {
                                    name: "Autoscaler__Apis__Kubernetes__Url",
                                    value: "https://kubernetes"
                                },
                                {
                                    name: "Autoscaler__Apis__Kubernetes__Mock",
                                    value: "false"
                                },
                                {
                                    name: "Autoscaler__Apis__Prometheus__Url",
                                    value: "http://prometheus-server"
                                },
                                {
                                    name: "Autoscaler__Apis__Prometheus__Mock",
                                    value: "false"
                                },
                                {
                                    name: "Autoscaler__Database__Database",
                                    value: #{dbName}
                                },
                                {
                                    name: "Autoscaler__Database__User",
                                    value: #{dbUser}
                                },
                                {
                                    name: "Autoscaler__Database__Password",
                                    value: #{dbPassword}
                                },
                                {
                                    name: "Autoscaler__Database__Hostname",
                                    value: #{dbHost}
                                },
                                {
                                    name: "Autoscaler__Database__Port",
                                    value: #{show dbPort}
                                },
                                {
                                    name: "Autoscaler__Port",
                                    value: #{show autoscalerPort}
                                },
                                {
                                    name: "Autoscaler__Runner__Start",
                                    value: "false"
                                },
                                {
                                    name: "Logging__LogLevel__Autoscaler",
                                    value: "Debug"
                                }
                            ],
                            image: "ghcr.io/aau-p9s/autoscaler:latest",
                            name: "autoscaler",
                            ports: [
                                {
                                    containerPort: #{autoscalerPort}
                                }
                            ],
                            resources: {
                                limits: {
                                    cpu: "1000m",
                                    memory: "2000Mi"
                                },
                                requests: {
                                    cpu: "500m",
                                    memory: "1000Mi"
                                }
                            },
                            volumeMounts: []
                        },
                        {
                            env: [
                                {
                                    name: "FORECASTER__PGSQL__DATABASE",
                                    value: #{dbName}
                                },
                                {
                                    name: "FORECASTER__PGSQL__USER",
                                    value: #{dbUser}
                                },
                                {
                                    name: "FORECASTER__PGSQL__PASSWORD",
                                    value: #{dbPassword}
                                },
                                {
                                    name: "FORECASTER__PGSQL__ADDR",
                                    value: #{dbHost}
                                },
                                {
                                    name: "FORECASTER__PGSQL__PORT",
                                    value: #{show dbPort}
                                },
                                {
                                    name: "FORECASTER__PORT",
                                    value: #{show forecasterPort}
                                },
                                {
                                    name: "FORECASTER__TRAIN__TIMEOUT",
                                    value: "30"
                                },
                                {
                                    name: "FORECASTER__ENABLE__GPU",
                                    value: "0"
                                }
                            ],
                            image: "ghcr.io/aau-p9s/forecaster:latest",
                            name: "forecaster",
                            ports: [
                                {
                                    containerPort: #{forecasterPort}
                                }
                            ],
                            resources: {
                                limits: {
                                    cpu: "1000m",
                                    memory: "2000Mi"
                                },
                                requests: {
                                    cpu: "500m",
                                    memory: "1000Mi"
                                }
                            },
                            volumeMounts: []

                        }
                    ],
                    serviceAccountName: "autoscaler",
                    volumes: []
                }
            }
        }
    }
|]]
