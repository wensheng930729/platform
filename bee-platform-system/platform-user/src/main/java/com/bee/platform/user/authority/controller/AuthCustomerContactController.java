package com.bee.platform.user.authority.controller;


import io.swagger.annotations.Api;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * <p>
 *  前端控制器
 * </p>
 *
 * @author hongchuan.He
 * @since 2019-05-20
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "authCustomerContact", tags = "新权限：客户与联系人相关的")
@RequestMapping("/authCustomerContact")
public class AuthCustomerContactController {

}

