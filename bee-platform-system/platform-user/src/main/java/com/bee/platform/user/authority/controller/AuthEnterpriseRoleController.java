package com.bee.platform.user.authority.controller;


import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.dto.AuthEnterpriseRoleTreeDTO;
import com.bee.platform.user.authority.dto.EnterpriseUrlDTO;
import com.bee.platform.user.authority.service.AuthEnterpriseRoleService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * <p>
 * 企业与角色（角色或功能的关联表）的中间表 前端控制器
 * </p>
 *
 * @author LILIANG
 * @since 2019-05-20
 */
@RestController
@RequestMapping("/authEnterpriseRole")
@Api(value = "authEnterpriseRole", tags = "新权限：企业应用功能管理表")
public class AuthEnterpriseRoleController {

    @Autowired
    private AuthEnterpriseRoleService enterpriseRoleService;


    @ApiOperation(value = "查询企业下的所有资源树")
    @PostMapping("/getEnterpriseRoleTreeList")
    public ResponseResult<List<AuthEnterpriseRoleTreeDTO>> getEnterpriseRoleTreeList(@RequestParam("enterpriseId") Integer enterpriseId) {
        return enterpriseRoleService.getEnterpriseRoleTreeList(enterpriseId);
    }

    @ApiOperation(value = "查询企业下的接口url")
    @GetMapping("/getEnterpriseInterfaceUri")
    public ResponseResult<List<EnterpriseUrlDTO>> getEnterpriseInterfaceUri(@RequestParam("enterpriseId") Integer enterpriseId, @RequestParam("subSys") String subSys) {

        List<EnterpriseUrlDTO> dto = enterpriseRoleService.getEnterpriseInterfaceUri(enterpriseId, subSys);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

}

