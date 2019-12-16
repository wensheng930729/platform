package com.bee.platform.user.authority.controller;


import io.swagger.annotations.Api;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * <p>
 * 资源角色表 前端控制器
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-20
 */

@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "authRoleResource", tags = "新权限：资源角色相关接口")
@RequestMapping("/authRoleResource")
public class AuthRoleResourceController {

}

