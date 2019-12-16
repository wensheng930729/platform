package com.bee.platform.user.authority.service.impl;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.enums.SystemCodeType;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.DateUtils;
import com.bee.platform.user.authority.dao.mapper.TSystemCodeTMapper;
import com.bee.platform.user.authority.dto.SystemCodeDTO;
import com.bee.platform.user.authority.entity.TSystemCodeT;
import com.bee.platform.user.authority.rq.SubSystemRQ;
import com.bee.platform.user.authority.rq.SystemCodeQueryRQ;
import com.bee.platform.user.authority.service.TSystemCodeTService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * <p>
 * 系统码表 服务实现类
 * </p>
 *
 * @author chenjie123123
 * @since 2019-05-27
 */
@Slf4j
@Service
public class TSystemCodeTServiceImpl extends ServiceImpl<TSystemCodeTMapper, TSystemCodeT> implements TSystemCodeTService {

    @Autowired
    private TSystemCodeTMapper systemCodeTMapper;
    private static Integer ZERO = 0;

    /**
     * 分页查询子系统列表
     * @param pagination
     * @return
     */
    @Override
    public List<SystemCodeDTO> listSubSystem(SystemCodeQueryRQ rq, Pagination pagination) {
        Map<String,Object> map = new HashMap<>();
        if (!ObjectUtils.isEmpty(rq) && !StringUtils.isEmpty(rq.getText())){
            map.put("text",rq.getText());
        }
        if (!ObjectUtils.isEmpty(rq) && !StringUtils.isEmpty(rq.getCreateStartTime()) && DateUtils.isValidDate(rq.getCreateStartTime())){
            map.put("createStartTime",rq.getCreateStartTime()  + " 00:00:00");
        }
        if (!ObjectUtils.isEmpty(rq) && !StringUtils.isEmpty(rq.getCreateEndTime()) && DateUtils.isValidDate(rq.getCreateEndTime())){
            map.put("createEndTime",rq.getCreateEndTime()  + " 23:59:59");
        }
        List<TSystemCodeT> list = systemCodeTMapper.selectSubSystemByCondition(map,pagination);
        /*List<TSystemCodeT> list = systemCodeTMapper.selectPage(pagination,new EntityWrapper<TSystemCodeT>()
                .eq("sys_group_id", SystemCodeType.SUBSYSTEM.getCode()).and()
                .eq("status", Status.TRUE.getKey()));*/
        return BeanUtils.assemble(SystemCodeDTO.class,list);
    }

    /**
     * 添加子系统
     * @param userInfo
     * @param rq
     * @return
     */
    @Override
    public ResponseResult<ResCodeEnum> addSubSystem(AuthPlatformUserInfo userInfo, SubSystemRQ rq) {
        TSystemCodeT systemCodeT = new TSystemCodeT()
                .setSysGroupId(SystemCodeType.SUBSYSTEM.getCode())
                .setSysCode(rq.getSysCode())
                .setSysCodeVal(rq.getSysCodeVal())
                .setSysCodeDesc(rq.getSysCodeDesc())
                .setStatus(Status.TRUE.getKey().toString())
                .setCreateTime(new Date())
                .setUpdateTime(new Date());
        if (!StringUtils.isEmpty(rq.getId())){
            if (systemCodeTMapper.updateById(systemCodeT.setId(Integer.valueOf(rq.getId()))) <= ZERO){
                log.error("修改子系统失败,调用{}类{}方法出错","TSystemCodeTServiceImpl","addSubSystem()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERROR_SYSTEM);
            }
        }else {
            if (systemCodeTMapper.insert(systemCodeT) <= ZERO){
                log.error("添加子系统失败,调用{}类{}方法出错","TSystemCodeTServiceImpl","addSubSystem()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERROR_SYSTEM);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 删除子系统
     * @param userInfo
     * @param id
     * @return
     */
    @Override
    public ResponseResult<ResCodeEnum> deleteSubSystem(AuthPlatformUserInfo userInfo, String id) {
        if (systemCodeTMapper.updateById(new TSystemCodeT()
                .setId(Integer.valueOf(id)).setStatus(Status.FALSE.getKey().toString()).setUpdateTime(new Date())) <= ZERO){
            log.error("删除子系统失败,调用{}类{}方法出错","TSystemCodeTServiceImpl","deleteSubSystem()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERROR_SYSTEM);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
}
