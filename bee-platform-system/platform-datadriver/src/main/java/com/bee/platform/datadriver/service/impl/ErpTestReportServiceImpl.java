package com.bee.platform.datadriver.service.impl;

import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.DateUtils;
import com.bee.platform.datadriver.dao.mapper.ErpTestReportMapper;
import com.bee.platform.datadriver.dao.mapper.ErpWarehousingOrderMapper;
import com.bee.platform.datadriver.dto.ErpTestReportDTO;
import com.bee.platform.datadriver.dto.ErpTestReportDetailDTO;
import com.bee.platform.datadriver.entity.ErpTestReport;
import com.bee.platform.datadriver.entity.ErpWarehousingOrder;
import com.bee.platform.datadriver.rq.ErpGetOneTestReportRQ;
import com.bee.platform.datadriver.rq.ErpTestReportDetailRQ;
import com.bee.platform.datadriver.rq.TestReportDetailRQ;
import com.bee.platform.datadriver.rq.TestReportQueryRQ;
import com.bee.platform.datadriver.service.ErpTestReportService;
import com.bee.platform.user.authority.dto.AuthEnterpriseFlatDTO;
import com.bee.platform.user.service.feign.AuthEnterpriseFeignClient;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * <p>
 * 化验单 服务实现类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Slf4j
@Service
public class ErpTestReportServiceImpl extends ServiceImpl<ErpTestReportMapper, ErpTestReport> implements ErpTestReportService {

    @Autowired
    private ErpTestReportMapper testReportMapper;
    @Autowired
    private AuthEnterpriseFeignClient authEnterpriseFeignClient;
    @Autowired
    private ErpWarehousingOrderMapper warehousingOrderMapper;

    private static Integer ZERO = 0;

    private static Integer ONE = 1;
    private static Integer TWO = 2;
    private static Integer THREE = 3;

    /**
     * 分页查询化验单列表
     * @param pagination
     * @param rq
     * @return
     */
    @Override
    public List<ErpTestReportDTO> listTestReport(Pagination pagination, TestReportQueryRQ rq, Integer companyId) {
        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByCompanyId(companyId).getObject();
        if (org.springframework.util.CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
            log.error("未查询到当前用户企业信息！ 类：{} 方法：{}", "ErpTestReportServiceImpl", "listTestReport");
            return Lists.newArrayList();
        }
        List<Integer> enterpriseIds = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
        if (!StringUtils.isEmpty(rq.getCreateStartTime()) && DateUtils.isValidDate(rq.getCreateStartTime())){
            rq.setCreateStartTime(rq.getCreateStartTime() + " 00:00:00");
        }
        if (!StringUtils.isEmpty(rq.getCreateEndTime()) && DateUtils.isValidDate(rq.getCreateEndTime())){
            rq.setCreateEndTime(rq.getCreateEndTime() + " 23:59:59");
        }
        rq.setList(enterpriseIds);
        List<ErpTestReport> reports = testReportMapper.selectReportsByCondition(rq,pagination);
        List<ErpTestReportDTO> dtos = BeanUtils.assemble(ErpTestReportDTO.class,reports);
        for (ErpTestReportDTO dto : dtos){
            List<ErpTestReportDetailDTO> detailDTO = (List<ErpTestReportDetailDTO>)JSONArray
                    .parseArray(dto.getResult(), ErpTestReportDetailDTO.class);
            List<ErpTestReportDetailDTO> detailDTOShow = new ArrayList<>();
            for (ErpTestReportDetailDTO d : detailDTO){
                if (Status.TRUE.getKey().equals(d.getShow())){
                    detailDTOShow.add(d);
                }
            }
            dto.setDetailDTO(detailDTOShow);
        }
        return dtos;
    }

    /**
     * 删除化验单
     * @param id
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> deleteTestReport(AuthPlatformUserInfo userInfo, String id) {
        if (testReportMapper.updateById(new ErpTestReport()
                .setId(Integer.valueOf(id)).setDeleted(Status.TRUE.getKey())) < ZERO){
            log.error("删除化验单失败,调用{}类{}方法出错","ErpTestReportServiceImpl","deleteTestReport()");
            throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_TEST_REPORT_DETAIL_DELETE_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,Integer.valueOf(id));
    }

    /**
     * 查询化验单详细信息
     * @param id
     * @return
     */
    @Override
    public ResponseResult<ErpTestReportDTO> getTestReport(String id) {
        /*ErpTestReport erpTestReport = testReportMapper.selectById(new ErpTestReport()
                .setId(Integer.valueOf(id)).setDeleted(Status.FALSE.getKey()));*/
        //ErpTestReportDTO erpTestReportDTO = BeanUtils.copyProperties(erpTestReport,ErpTestReportDTO.class);
        ErpTestReportDTO erpTestReportDTO = testReportMapper.selectDetailById(id);
        List<ErpTestReportDetailDTO> detailDTO = (List<ErpTestReportDetailDTO>)JSONArray
                .parseArray(erpTestReportDTO.getResult(), ErpTestReportDetailDTO.class);
        erpTestReportDTO.setDetailDTO(detailDTO);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,erpTestReportDTO);
    }

    /**
     * 保存化验单
     * @param userInfo
     * @param rq
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> saveTestReport(AuthPlatformUserInfo userInfo, TestReportDetailRQ rq) {
        ErpTestReport testReport = new ErpTestReport();
        JSONArray json = new JSONArray();
        if (!CollectionUtils.isEmpty(rq.getDetailList())){
            for(ErpTestReportDetailRQ r : rq.getDetailList()){
                JSONObject jo = new JSONObject();
                jo.put("testItem", r.getTestItem());
                jo.put("testValue", r.getTestValue());
                jo.put("show", r.getShow());
                json.add(jo);
            }
        }
        BeanUtils.copyProperties(rq,testReport);
        testReport.setResult(json.toJSONString());
        if (testReport.getId() == null){
            if (testReportMapper.selectCount(new EntityWrapper<>(new ErpTestReport()
                    .setCompany(rq.getCompany()).setCode(rq.getCode()).setDeleted(Status.FALSE.getKey()))) > ZERO){
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_TEST_REPORT_REPEAT);
            }
            if (testReportMapper.insert(testReport.setDeleted(Status.FALSE.getKey())
                    .setCreateUser(userInfo.getId())
                    .setCreateTime(new Date())) <= ZERO){
                log.error("新增化验单失败,调用{}类{}方法出错","ErpTestReportServiceImpl","saveTestReport()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_TEST_REPORT_ADD_FAILED);
            }
        }else {
            if (testReportMapper.updateById(testReport) <= ZERO){
                log.error("修改化验单失败,调用{}类{}方法出错","ErpTestReportServiceImpl","saveTestReport()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_TEST_REPORT_UPDATE_FAILED);
            }
        }
        // 修改成品入库单化验单id
        if (THREE.equals(rq.getTestTypeCategory()) && rq.getOrderNo() != null){
            if (warehousingOrderMapper.updateById(new ErpWarehousingOrder()
                    .setId(rq.getOrderNo())
                    .setTestReportId(testReport.getId())
                    .setTestReportCode(testReport.getCode())) <= ZERO){
                log.error("修改入库单失败,调用{}类{}方法出错","ErpTestReportServiceImpl","saveTestReport()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_WAREHOUSING_ORDER_UPDATE_FAILED);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,testReport.getId());
    }

    /**
     * 根据入库单查询化验单详细信息
     * @param rq
     * @return
     */
    @Override
    public ResponseResult<ErpTestReport> getTestReportByWarehousingOrder(ErpGetOneTestReportRQ rq) {
        List<ErpTestReport> erpTestReports = testReportMapper.selectList(new EntityWrapper<>(new ErpTestReport()
                .setProduct(rq.getProduct())
                .setCompany(rq.getCompany())
                .setBoilerId(rq.getBoilerId())
                .setShiftsId(rq.getShiftsId())
                .setProductDate(rq.getProductDate())));
        ErpTestReport report = new ErpTestReport();
        if (!CollectionUtils.isEmpty(erpTestReports)){
            report = erpTestReports.get(0);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,report);
    }
}
