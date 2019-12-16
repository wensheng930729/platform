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
import com.bee.platform.datadriver.dao.mapper.ErpOperationLogMapper;
import com.bee.platform.datadriver.dao.mapper.ErpTestReportMapper;
import com.bee.platform.datadriver.dao.mapper.ErpWarehousingOrderMapper;
import com.bee.platform.datadriver.dto.ErpTestReportDTO;
import com.bee.platform.datadriver.dto.ErpTestReportDetailDTO;
import com.bee.platform.datadriver.entity.ErpOperationLog;
import com.bee.platform.datadriver.entity.ErpTestReport;
import com.bee.platform.datadriver.entity.ErpWarehousingOrder;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.rq.ErpGetOneTestReportRQ;
import com.bee.platform.datadriver.rq.ErpTestReportDetailRQ;
import com.bee.platform.datadriver.rq.TestReportDetailRQ;
import com.bee.platform.datadriver.rq.TestReportQueryRQ;
import com.bee.platform.datadriver.service.ErpOperationLogService;
import com.bee.platform.datadriver.service.ErpTestReportService;
import com.bee.platform.datadriver.support.OperateType;
import com.bee.platform.user.authority.dto.AuthEnterpriseFlatDTO;
import com.bee.platform.user.service.feign.AuthEnterpriseFeignClient;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.apache.commons.lang.StringUtils;
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
    private ErpOperationLogMapper operationLogMapper;
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
    public List<ErpTestReportDTO> listTestReport(Pagination pagination, TestReportQueryRQ rq, AuthPlatformUserInfo userInfo) {
        rq.setCompany(userInfo.getOrgId());
        List<ErpTestReport> reports = testReportMapper.selectReportsByCondition(rq,pagination);
        List<ErpTestReportDTO> dtos = BeanUtils.assemble(ErpTestReportDTO.class,reports);
        return this.cvtMainValue(dtos);
    }

    /**
     * 拼接主属性
     * @param dtos
     * @return
     */
    private List<ErpTestReportDTO> cvtMainValue(List<ErpTestReportDTO> dtos) {
        for (ErpTestReportDTO dto : dtos){
            List<ErpTestReportDetailDTO> detailDTO = (List<ErpTestReportDetailDTO>)JSONArray
                    .parseArray(dto.getResult(), ErpTestReportDetailDTO.class);
            List<String> mainItem = new ArrayList<>();
            List<String> mainItemValue = new ArrayList<>();
            for (ErpTestReportDetailDTO d : detailDTO){
                if (Status.TRUE.getKey().equals(d.getShow())){
                    if (StringUtils.isNotEmpty(d.getTestItem())){
                        mainItem.add(d.getTestItem());
                    }
                    if (StringUtils.isNotEmpty(d.getTestValue())){
                        mainItemValue.add(d.getTestValue());
                    }

                }
            }
            if (!StringUtils.isEmpty(dto.getBatchName()) && !StringUtils.isEmpty(dto.getProductName())){
                dto.setProductName(dto.getProductName() + "-" + dto.getBatchName());
            }
            dto.setMainItem(StringUtils.join(mainItem,"/"));
            dto.setMainItemValue(StringUtils.join(mainItemValue,"/"));
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
        if (checkValue(rq.getDetailList())){
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_TEST_REPORT_MAIN_VALUE_EMPTY);
        }
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
            // 插入操作日志
            insertOperationLog(testReport,userInfo,OperateType.ADD.getMsg());
        }else {
            if (testReportMapper.updateById(testReport) <= ZERO){
                log.error("修改化验单失败,调用{}类{}方法出错","ErpTestReportServiceImpl","saveTestReport()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_TEST_REPORT_UPDATE_FAILED);
            }
            // 插入操作日志
            insertOperationLog(testReport,userInfo,OperateType.EDIT.getMsg());
        }
        // 修改成品入库单化验单id
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,testReport.getId());
    }

    /**
     * 插入操作日志
     * @param testReport
     * @param userInfo
     * @param msg
     */
    private void insertOperationLog(ErpTestReport testReport, AuthPlatformUserInfo userInfo, String msg) {
        // 插入操作日志
        if(operationLogMapper.insert(new ErpOperationLog()
                .setBusinessId(testReport.getId())
                .setCompanyId(userInfo.getOrgId())
                .setBusinessType(EnumBusinessType.TEST_REPORT.getCode())
                .setOperateMsg(msg)
                .setOperator(userInfo.getId())
                .setOperatorName(userInfo.getName())
                .setOperateTime(new Date())) <= ZERO){
            log.error("新增化验单操作日志失败,调用{}类{}方法出错","ErpTestReportServiceImpl","saveTestReport()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_TEST_REPORT_LOG_ADD_FAILED);
        }
    }

    /**
     * 校验主属性是否为空
     * @param detailList
     * @return
     */
    private boolean checkValue(List<ErpTestReportDetailRQ> detailList) {
        for (ErpTestReportDetailRQ rq : detailList){
            if (ONE.equals(rq.getShow()) && StringUtils.isEmpty(rq.getTestValue())){
                return true;
            }
        }
        return false;
    }
}
