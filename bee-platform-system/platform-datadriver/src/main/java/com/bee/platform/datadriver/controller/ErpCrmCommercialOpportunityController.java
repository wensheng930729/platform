package com.bee.platform.datadriver.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.datadriver.entity.ErpCode;
import com.bee.platform.datadriver.entity.ErpCrmCommercialOpportunity;
import com.bee.platform.datadriver.dto.ErpCrmRankToCommercialDTO;
import com.bee.platform.datadriver.dto.ErpCrmSalesRankDTO;
import com.bee.platform.datadriver.dto.ErpCrmCommercialOpportunityDTO;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.rq.CommercialOpportunityQueryRQ;
import com.bee.platform.datadriver.rq.ErpCrmCommercialOpportunityAddRQ;
import com.bee.platform.datadriver.rq.ErpCrmCommercialOpportunityDeleteRQ;
import com.bee.platform.datadriver.rq.ErpCrmCommercialOpportunityUpdateRQ;
import com.bee.platform.datadriver.service.ErpCrmCommercialOpportunityService;
import com.bee.platform.datadriver.support.OperateType;
import com.bee.platform.datadriver.support.annotation.Log;
import com.bee.platform.user.service.feign.AuthUserFeignClient;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.util.IOUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;


import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import java.io.IOException;
import java.io.OutputStream;
import java.time.LocalDate;
import java.util.List;

/**
 * <p>
 * 商机信息 前端控制器
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-24
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpCrmCommercialOpportunity", tags = "crm商机信息相关接口")
@RequestMapping("/erpCrmCommercialOpportunity")
public class ErpCrmCommercialOpportunityController {

    @Autowired
    private ErpCrmCommercialOpportunityService crmCommercialOpportunityService;

    @Autowired
    private AuthUserFeignClient userInfoFeignClient;

    @Autowired
    private UserInfoUtils userInfoUtils;
    
    @PostMapping("/listCommercialOpportunity")
    @ApiOperation(value = "分页查询商机信息列表")
    public ResponseResult<List<ErpCrmCommercialOpportunity>> listCommercialOpportunity(HttpServletRequest request, @RequestBody CommercialOpportunityQueryRQ rq, Page page) {
        Integer companyId = Integer.valueOf(WebUtils.getParam("company", request));
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(companyId)) {
            log.error("header获取公司id失败");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER_NO_COMPANY);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        List<ErpCrmCommercialOpportunity> list = crmCommercialOpportunityService.listCommercialOpportunity(pagination, rq, companyId,userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list, PageUtils.transToPage(pagination));
    }
    
    
    @ApiOperation(value = "商机客户列表导出excel")
    @PostMapping("/exportBOC")
    public ResponseResult<String> exportBOC(HttpServletRequest request, HttpServletResponse response,@RequestBody @Valid CommercialOpportunityQueryRQ rq){
    	 Integer companyId = Integer.valueOf(WebUtils.getParam("company", request));
    	 AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
    	 if (ObjectUtils.isEmpty(companyId)) {
             log.error("header获取公司id失败");
             return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER_NO_COMPANY);
         }
    	 Workbook workbook = crmCommercialOpportunityService.exportBOC(companyId,rq,userInfo);
    	 // 输出Excel文件
         String fileName = LocalDate.now() + "_BOC.xls";
         response.setContentType("application/force-download");
         response.setHeader("Content-Disposition", "attachment;filename=" + fileName);
         response.setContentType("application/vnd.ms-excel;charset=utf-8");
         OutputStream output = null;
         try {
             output = response.getOutputStream();
             workbook.write(output);
         } catch (IOException e1) {
             e1.printStackTrace();
         } finally {
             IOUtils.closeQuietly(output);
         }
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @PostMapping("/getCustomerType")
    @ApiOperation(value = "查询所有客户类型")
    public ResponseResult<List<ErpCode>> getCustomerType(HttpServletRequest request, @RequestParam String customerType) {
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, crmCommercialOpportunityService.getCustomerType(customerType));
    }

    @PostMapping("/getcustomerCurrentStage")
    @ApiOperation(value = "查询客户当前阶段")
    public ResponseResult<List<ErpCode>> getcustomerCurrentStage(@RequestParam String code) {
        return crmCommercialOpportunityService.getcustomerCurrentStage(code);
    }


    @GetMapping("/getCommercialOpportunityUserId")
    @ApiOperation(value = "查询当前销售员的客户信息")
    public ResponseResult<ErpCrmCommercialOpportunity> getCommercialOpportunityUserId(HttpServletRequest request) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId()) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("无法获取用户信息");
        }
        return crmCommercialOpportunityService.getCommercialOpportunityUserId(userInfo);
    }

    @ApiOperation(value = "查询【销售人员】销售排行榜")
    @GetMapping("/getSellerSalesRank")
    public ResponseResult<List<ErpCrmSalesRankDTO>> getSellerSalesRank(HttpServletRequest request, Page page) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);

        Integer companyId = Integer.valueOf(WebUtils.getParam("company", request));
        if (ObjectUtils.isEmpty(companyId)) {
            log.error("header获取公司id失败");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER_NO_COMPANY);
        }

        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return crmCommercialOpportunityService.getSellerSalesRank(userInfo.getId(), companyId, page);

    }


    @ApiOperation(value = "查询【管理员】销售排行榜")
    @GetMapping("/getManagerSalesRank")
    public ResponseResult<List<ErpCrmSalesRankDTO>> getManagerSalesRank(HttpServletRequest request, Page page) {
        Integer companyId = Integer.valueOf(WebUtils.getParam("company", request));

        if (ObjectUtils.isEmpty(companyId)) {
            log.error("header获取公司id失败");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER_NO_COMPANY);
        }

        return crmCommercialOpportunityService.getManagerSalesRank(companyId, page);

    }

    @ApiOperation(value = "导出销售排行榜excel")
    @GetMapping("/exportSalesRank")
    public ResponseResult<String> exportSalesRank(HttpServletRequest request, HttpServletResponse response){
        Integer companyId = Integer.valueOf(WebUtils.getParam("company", request));

        if (ObjectUtils.isEmpty(companyId)) {
            log.error("header获取公司id失败");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER_NO_COMPANY);
        }
        Workbook workbook = crmCommercialOpportunityService.exportSalesRank(companyId);
        // 输出Excel文件
        String fileName = LocalDate.now() + "_SalesRank.xls";
        response.setContentType("application/force-download");
        response.setHeader("Content-Disposition", "attachment;filename=" + fileName);
        response.setContentType("application/vnd.ms-excel;charset=utf-8");
        OutputStream output = null;
        try {
            output = response.getOutputStream();
            workbook.write(output);
        } catch (IOException e1) {
            e1.printStackTrace();
        } finally {
            IOUtils.closeQuietly(output);
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }



    @ApiOperation(value = "查询【销售排行榜】到【商机客户】列表")
    @GetMapping("/getCrmRankToCommercial")
    public ResponseResult<List<ErpCrmRankToCommercialDTO>> getCrmRankToCommercial(HttpServletRequest request, @RequestParam Integer saleUserId, @RequestParam Integer type, Page page) {
        Integer companyId = Integer.valueOf(WebUtils.getParam("company", request));

        if (ObjectUtils.isEmpty(companyId)) {
            log.error("header获取公司id失败");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER_NO_COMPANY);
        }
        return crmCommercialOpportunityService.getCrmRankToCommercial(saleUserId, type, companyId, page);

    }

    @GetMapping("/getCommercialOpportunity")
    @ApiOperation(value = "根据id查询商机信息")
    public ResponseResult<ErpCrmCommercialOpportunityDTO> getCommercialOpportunity(@RequestParam Integer id) {
        if (id == null) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return crmCommercialOpportunityService.getCommercialOpportunity(id);
    }


    @Log(businessType = EnumBusinessType.COMMERCIAL_OPPORTUNITY, operateType = OperateType.ADD)
    @PostMapping("/addCommercialOpportunity")
    @ApiOperation(value = "新增商机信息")
    public ResponseResult<Integer> addCommercialOpportunity(HttpServletRequest request, @RequestBody @Valid ErpCrmCommercialOpportunityAddRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient
                .simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request))
                .getObject();

        Integer companyId = Integer.valueOf(WebUtils.getParam("company", request));

        if (ObjectUtils.isEmpty(companyId)) {
            log.error("header获取公司id失败");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER_NO_COMPANY);
        }
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return crmCommercialOpportunityService.addCommercialOpportunity(rq, userInfo,companyId);
    }

    @Log(businessType = EnumBusinessType.COMMERCIAL_OPPORTUNITY, operateType = OperateType.EDIT)
    @PostMapping("/updateCommercialOpportunity")
    @ApiOperation(value = "修改商机信息")
    public ResponseResult<Integer> updateCommercialOpportunity(HttpServletRequest request, @RequestBody @Valid ErpCrmCommercialOpportunityUpdateRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient
                .simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request))
                .getObject();

        Integer companyId = Integer.valueOf(WebUtils.getParam("company", request));

        if (ObjectUtils.isEmpty(companyId)) {
            log.error("header获取公司id失败");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER_NO_COMPANY);
        }
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return crmCommercialOpportunityService.updateCommercialOpportunity(rq, userInfo,companyId);
    }


	 @PostMapping("/deleted")
   @ApiOperation(value = "删除商机客户")
   public ResponseResult deleted(HttpServletRequest request, @RequestBody ErpCrmCommercialOpportunityDeleteRQ deleteRQ){
   	 AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
   	return crmCommercialOpportunityService.deleted(userInfo, deleteRQ);
   	
   }

}

