package com.bee.platform.customer.controller;


import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.customer.entity.AuthCustomerRole;
import com.bee.platform.user.authority.rq.ChangeOrderNumRQ;
import com.bee.platform.user.authority.rq.CustomerRelationRoleRQ;
import com.bee.platform.customer.service.AuthCustomerRoleService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

/**
 * <p>
 * 客户关联角色/功能/应用  前端控制器
 * </p>
 *
 * @author cheng.ke
 * @since 2019-05-20
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/authCustomerRole")
@Api(value = "authCustomerRole" ,tags = "客户关联角色/功能/应用相关接口")
public class AuthCustomerRoleController {


    @Autowired
    private AuthCustomerRoleService authCustomerRoleService;

/*    @ApiOperation(value = "客户关联角色/功能/应用")
    @PostMapping("/customerRelationRole")
    public ResponseResult customerRelationRole(@RequestParam() Integer createId, @RequestBody @Valid CustomerRelationRoleRQ rq){

        if(ObjectUtils.isEmpty(rq)){
            log.error("参数不全");
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }

        return authCustomerRoleService.changeCustomerRole(createId,rq);
    }*/

    @ApiOperation(value = "修改排序值")
    @PostMapping("/changeOrderNum")
    public ResponseResult changeOrderNum(@RequestBody @Valid ChangeOrderNumRQ rq){
        if(ObjectUtils.isEmpty(rq)){
            log.error("参数不全");
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        authCustomerRoleService.updateById(new AuthCustomerRole().setId(rq.getId()).setOrderNum(rq.getNum()));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

}

