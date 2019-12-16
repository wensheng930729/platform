package com.bee.platform.datadriver.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.datadriver.dto.ErpTestReportDTO;
import com.bee.platform.datadriver.entity.ErpTestReport;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.rq.ErpGetOneTestReportRQ;
import com.bee.platform.datadriver.rq.TestReportDetailRQ;
import com.bee.platform.datadriver.rq.TestReportQueryRQ;
import com.bee.platform.datadriver.service.ErpTestReportService;
import com.bee.platform.datadriver.support.OperateType;
import com.bee.platform.datadriver.support.annotation.Log;
import com.bee.platform.user.service.feign.AuthUserFeignClient;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;
import java.util.Objects;

/**
 * <p>
 * 化验单 前端控制器
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpTestReport", tags = "erp化验单相关接口")
@RequestMapping("/erpTestReport")
public class ErpTestReportController {

    @Autowired
    private ErpTestReportService testReportService;
    @Autowired
    private AuthUserFeignClient userInfoFeignClient;

    @PostMapping("/listTestReport")
    @ApiOperation(value = "分页查询化验单列表")
    public ResponseResult<List<ErpTestReportDTO>> listAppRolesBack(HttpServletRequest request, @RequestBody TestReportQueryRQ rq , Page page){
        /*String sysToken = WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request);
        if (StringUtils.isEmpty(sysToken)){
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }*/
        Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));
        if (Objects.isNull(companyId) && Objects.isNull(rq.getCompany())){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        List<ErpTestReportDTO> list = testReportService.listTestReport(pagination,rq,companyId);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list,PageUtils.transToPage(pagination));
    }

    @GetMapping("/getTestReport")
    @ApiOperation(value = "查询化验单详细信息")
    public ResponseResult<ErpTestReportDTO> getTestReport(HttpServletRequest request, @RequestParam String id){
        if (StringUtils.isEmpty(id)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return testReportService.getTestReport(id);
    }

    @GetMapping("/deleteTestReport")
    @Log(businessType= EnumBusinessType.TEST_REPORT, operateType = OperateType.DELETE)
    @ApiOperation(value = "删除化验单")
    public ResponseResult<Integer> deleteTestReport(HttpServletRequest request, @RequestParam String id){
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (StringUtils.isEmpty(id)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return testReportService.deleteTestReport(userInfo,id);
    }

    @PostMapping("/saveTestReport")
    //@Log(businessType= EnumBusinessType.TEST_REPORT, operateType = OperateType.ADD)
    @ApiOperation(value = "保存化验单")
    public ResponseResult<Integer> saveTestReport(HttpServletRequest request, @RequestBody TestReportDetailRQ rq){
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return testReportService.saveTestReport(userInfo,rq);
    }


    @PostMapping("/getTestReportByWarehousingOrder")
    @ApiOperation(value = "根据入库单查询化验单详细信息")
    public ResponseResult<ErpTestReport> getTestReportByWarehousingOrder(HttpServletRequest request, @RequestBody ErpGetOneTestReportRQ rq){
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return testReportService.getTestReportByWarehousingOrder(rq);
    }

}

