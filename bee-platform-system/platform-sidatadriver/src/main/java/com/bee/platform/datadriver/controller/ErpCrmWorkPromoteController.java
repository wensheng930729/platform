
package com.bee.platform.datadriver.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.datadriver.entity.ErpCrmWorkPromote;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.rq.WorkPromoteSaveRQ;
import com.bee.platform.datadriver.service.ErpCrmWorkPromoteService;
import com.bee.platform.datadriver.support.OperateType;
import com.bee.platform.datadriver.support.annotation.Log;
import com.bee.platform.user.service.feign.AuthUserFeignClient;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * <p>
 * 工作推进 前端控制器
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-24
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpCrmWorkPromote", tags = "crm工作推进相关接口")
@RequestMapping("/erpCrmWorkPromote")
public class ErpCrmWorkPromoteController {

    @Autowired
    private ErpCrmWorkPromoteService workPromoteService;
    @Autowired
    private AuthUserFeignClient userInfoFeignClient;

    @PostMapping("/listCrmWorkPromote")
    @ApiOperation(value = "查询工作推进列表")
    public ResponseResult<List<ErpCrmWorkPromote>> listCrmWorkPromote(@RequestParam Integer commercialId){
        List<ErpCrmWorkPromote> list = workPromoteService.listCrmWorkPromote(commercialId);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list);
    }

    @PostMapping("/addCrmWorkPromote")
    @ApiOperation(value = "新增工作推进")
    @Log(businessType = EnumBusinessType.COMMERCIAL_OPPORTUNITY, operateType = OperateType.ADD_CUSTOMER_ANALYSIS)
    public ResponseResult<Integer> addCrmWorkPromote(HttpServletRequest request, @RequestBody WorkPromoteSaveRQ rq){
        AuthPlatformUserInfo userInfo = userInfoFeignClient
                .simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request))
                .getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return workPromoteService.addCrmWorkPromote(rq,userInfo);
    }

    @GetMapping("/deleteCrmWorkPromote")
    @ApiOperation(value = "删除工作推进")
    @Log(businessType = EnumBusinessType.COMMERCIAL_OPPORTUNITY, operateType = OperateType.DELETE_CUSTOMER_ANALYSIS)
    public ResponseResult<Integer> deleteCrmWorkPromote(HttpServletRequest request, @RequestParam Integer id){
        AuthPlatformUserInfo userInfo = userInfoFeignClient
                .simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request))
                .getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (id == null) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return workPromoteService.deleteCrmWorkPromote(id,userInfo);
    }

}
