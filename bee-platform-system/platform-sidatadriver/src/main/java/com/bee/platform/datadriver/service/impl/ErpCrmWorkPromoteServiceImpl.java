
package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.datadriver.entity.ErpCrmCommercialOpportunity;
import com.bee.platform.datadriver.entity.ErpCrmWorkPromote;
import com.bee.platform.datadriver.dao.mapper.ErpCrmWorkPromoteMapper;
import com.bee.platform.datadriver.rq.WorkPromoteSaveRQ;
import com.bee.platform.datadriver.service.ErpCrmWorkPromoteService;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;

/**
 * <p>
 * 工作推进 服务实现类
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-24
 */
@Slf4j
@Service
public class ErpCrmWorkPromoteServiceImpl extends ServiceImpl<ErpCrmWorkPromoteMapper, ErpCrmWorkPromote> implements ErpCrmWorkPromoteService {

    @Autowired
    private ErpCrmWorkPromoteMapper workPromoteMapper;
    private static Integer ZERO = 0;

    /**
     * 查询工作推进
     * @param commercialId
     * @return
     */
    @Override
    public List<ErpCrmWorkPromote> listCrmWorkPromote(Integer commercialId) {
        return workPromoteMapper.selectList(new EntityWrapper<>(new ErpCrmWorkPromote()
                .setCommercialId(commercialId)
                .setDeleted(Status.FALSE.getKey()))
                .orderBy("create_time",true));
    }

    /**
     * 新增工作推进
     * @param rq
     * @param userInfo
     * @return
     */
    @Override
    public ResponseResult<Integer> addCrmWorkPromote(WorkPromoteSaveRQ rq, AuthPlatformUserInfo userInfo) {
        ErpCrmWorkPromote workPromote = new ErpCrmWorkPromote();
        BeanUtils.copyProperties(rq,workPromote);
        if (workPromoteMapper.insert(workPromote.setDeleted(Status.FALSE.getKey())
                .setCreateUser(userInfo.getId()).setCreateTime(new Date())
                .setUpdateUser(userInfo.getId()).setUpdateTime(new Date())) <= ZERO){
            log.error("新增工作推进失败,调用{}类{}方法出错","ErpCrmWorkPromoteServiceImpl","addCrmWorkPromote()");
            throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.CRM_WORK_PROMOTE_ADD_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,rq.getCommercialId());
    }

    /**
     * 删除工作推进
     * @param id
     * @param userInfo
     * @return
     */
    @Override
    public ResponseResult<Integer> deleteCrmWorkPromote(Integer id, AuthPlatformUserInfo userInfo) {
        ErpCrmWorkPromote workPromote = workPromoteMapper.selectById(new ErpCrmWorkPromote().setId(id)
                .setDeleted(Status.FALSE.getKey()));
        if (workPromoteMapper.updateById(new ErpCrmWorkPromote().setId(id)
                .setDeleted(Status.TRUE.getKey())) < ZERO){
            log.error("新增工作推进失败,调用{}类{}方法出错","ErpCrmWorkPromoteServiceImpl","deleteCrmWorkPromote()");
            throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.CRM_WORK_PROMOTE_DELETE_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,workPromote.getCommercialId());
    }
}
