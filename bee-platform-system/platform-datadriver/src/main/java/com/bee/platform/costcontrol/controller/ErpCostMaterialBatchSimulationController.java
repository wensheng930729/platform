package com.bee.platform.costcontrol.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.costcontrol.service.ErpCostMaterialBatchSimulationService;
import com.bee.platform.costcontroller.rq.ErpCostMaterialSimulationListRQ;
import com.bee.platform.costcontroller.rq.ErpCostMaterialSimulationRQ;
import com.bee.platform.costcontroller.rq.ErpCostSimulationResultStatusRQ;
import com.bee.platform.user.service.feign.AuthUserFeignClient;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.util.Objects;


/**
 * <p>
 * 料批模拟 前端控制器
 * </p>
 *
 * @author xin.huang
 * @since 2019-06-25
 */
@Slf4j
@RestController
@RequestMapping("/erpCostMaterialBatchSimulation")
@Api(value = "ErpCostMaterialBatchSimulationController", tags = "erp-料批模拟相关接口")
public class ErpCostMaterialBatchSimulationController {

    @Autowired
    private AuthUserFeignClient userInfoFeignClient;

    @Autowired
    private ErpCostMaterialBatchSimulationService simulationService;

    @Autowired
    private UserInfoUtils userInfoUtils;


    @PostMapping("/add")
    @ApiOperation(value = "新增")
    public ResponseResult add(HttpServletRequest request, @RequestBody ErpCostMaterialSimulationRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(request.getHeader("sysToken")).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return simulationService.add(userInfo, rq);
    }

    @PostMapping("/update")
    @ApiOperation(value = "编辑")
    public ResponseResult update(HttpServletRequest request, @RequestBody ErpCostMaterialSimulationRQ rq) {
        if (Objects.isNull(rq) || Objects.isNull(rq.getId())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(request.getHeader("sysToken")).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return simulationService.update(userInfo, rq);
    }

    @PostMapping("/updateStatus")
    @ApiOperation(value = "更新计算结果状态")
    public ResponseResult updateStatus(HttpServletRequest request, @RequestBody ErpCostSimulationResultStatusRQ rq) {
        if (Objects.isNull(rq) || StringUtils.isEmpty(rq.getId())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return simulationService.updateStatus(userInfo, rq);
    }

    @PostMapping("/delete")
    @ApiOperation(value = "删除")
    public ResponseResult delete(HttpServletRequest request, @RequestParam Integer id) {
        if (Objects.isNull(id)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return simulationService.deleteMaterial(id);
    }

    @PostMapping("/get")
    @ApiOperation(value = "查询料批模拟详情")
    public ResponseResult findInfo(HttpServletRequest request, @RequestParam Integer id) {
        if (Objects.isNull(id)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return simulationService.findInfo(id);
    }

    @PostMapping("/list")
    @ApiOperation(value = "条件查询料批模拟列表")
    public ResponseResult findList(HttpServletRequest request, @RequestBody ErpCostMaterialSimulationListRQ rq, Page page) {
        AuthPlatformUserInfo simpleUserInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(simpleUserInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));
        if (Objects.isNull(companyId) && Objects.isNull(rq.getCompanyId())){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return simulationService.findList(companyId, rq, pagination);
    }

    @PostMapping("/material/company")
    @ApiOperation(value = "根据公司id查询料批模拟列表")
    public ResponseResult findListByCompanyId(HttpServletRequest request, @RequestParam Integer companyId) {
        if (Objects.isNull(companyId)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return simulationService.findListByCompanyId(companyId);
    }

    @ApiIgnore
    @PostMapping("/material/calculateResult")
    @ApiOperation(value = "计算料批模拟结果")
    public ResponseResult calculateResult(@RequestBody @Valid ErpCostMaterialSimulationRQ rq) {
        return simulationService.calculateResult(rq);
    }

}
