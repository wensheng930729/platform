package com.bee.platform.user.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.ConstInfos;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.user.dao.mapper.LearnMapper;
import com.bee.platform.user.dto.LearnDTO;
import com.bee.platform.user.entity.Learn;
import com.bee.platform.user.service.EnterprisesUsersService;
import com.bee.platform.user.service.LearnService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.ArrayUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.Date;
import java.util.List;

/**
 * <p>
 * 服务实现类
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-03-04
 */
@Slf4j
@Service
public class LearnServiceImpl extends ServiceImpl<LearnMapper, Learn> implements LearnService {

    @Autowired
    private EnterprisesUsersService enterprisesUsersService;

    @Autowired
    private LearnMapper learnMapper;

    /**
     * 创建学习指南
     *
     * @param userInfo
     * @param title
     * @param content
     * @param type
     * @param depName
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult addLearn(AuthPlatformUserInfo userInfo, String title, String content, int type, String depName) {

        // 参数组装
        Learn learn = new Learn();
        learn.setTitle(title);
        learn.setContent(content);
        learn.setOrgId(userInfo.getOrgId());
        learn.setUserId(userInfo.getId());
        learn.setDepName(depName);
        learn.setCreateAt(new Date());
        learn.setUpdateAt(new Date());
        // 文章类型
        int insertType;
        switch (type) {
            case 0:
                insertType = ConstInfos.LEARN_TYPE.USE.ordinal();
                break;
            case 1:
                insertType = ConstInfos.LEARN_TYPE.TRAINING.ordinal();
                break;
            case 2:
                insertType = ConstInfos.LEARN_TYPE.PLATFORM_INFO.ordinal();
                break;
            case 3:
                insertType = ConstInfos.LEARN_TYPE.RULES.ordinal();
                break;
            default:
                throw new BusinessException(ResCodeEnum.ERROR_PARAMETER, ExceptionMessageEnum.LEARN_TYPE_ERROR);
        }
        learn.setType(insertType);
        // 创建学习指南
        if (!this.insert(learn)) {
            log.error("创建学习指南失败！调用{}的{}方法出错 ", "LearnServiceImpl", "addLearn()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.INSERT_LEARN_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }


    /**
     * 修改学习指南
     *
     * @param userInfo
     * @param id
     * @param title
     * @param content
     * @param type
     * @param depName
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult modifyLearn(AuthPlatformUserInfo userInfo, int id, String title, String content, int type, String depName) {

        //根据id查询此条学习指南记录
        Learn learn = this.selectById(id);
        if (ObjectUtils.isEmpty(learn)) {
            log.error("参数异常，查询不到相关学习指南！调用{}的{}方法出错 ", "LearnServiceImpl", "modifyLearn()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.LEARN_NOT_FOUND);
        }
        if (!ObjectUtils.isEmpty(learn.getOrgId()) && learn.getOrgId().equals(userInfo.getOrgId())) {
            learn.setUserId(userInfo.getId());
            learn.setTitle(title);
            learn.setContent(content);
            learn.setDepName(depName);
            learn.setUpdateAt(new Date());
            int[] types = {0, 1, 2, 3};
            if (ArrayUtils.contains(types, type)) {
                learn.setType(type);
                if (!this.updateById(learn)) {
                    log.error("修改学习指南失败！调用{}的{}方法出错 ", "LearnServiceImpl", "modifyLearn()");
                    throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.UPDATE_LEARN_FAILED);

                }
                return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);

            } else {
                log.error("学习指南类型错误！调用{}的{}方法出错 ", "LearnServiceImpl", "modifyLearn()");
                throw new BusinessException(ResCodeEnum.STUDY_GUIDE_TYPE, ExceptionMessageEnum.LEARN_TYPE_ERROR);
            }
        } else {
            log.error("该学习指南不属于该企业！调用{}的{}方法出错 ", "LearnServiceImpl", "modifyLearn()");
            throw new BusinessException(ResCodeEnum.STUDY_GUIDE_NOT_ENTERPRISE, ExceptionMessageEnum.STUDY_GUIDE_NOT_ENTERPRISE);
        }

    }


    /**
     * 删除学习指南
     *
     * @param userInfo
     * @param id
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult deleteLearn(AuthPlatformUserInfo userInfo, int id) {

        //根据id查询此条学习指南记录
        Learn learn = this.selectById(id);
        if (ObjectUtils.isEmpty(learn)) {
            log.error("参数异常，查询不到相关学习指南！调用{}的{}方法出错 ", "LearnServiceImpl", "deleteLearn()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.LEARN_NOT_FOUND);
        }

        if (!ObjectUtils.isEmpty(learn.getOrgId()) && learn.getOrgId().equals(userInfo.getOrgId())) {
            if (!this.deleteById(id)) {
                log.error("删除学习指南失败！调用{}的{}方法出错 ", "LearnServiceImpl", "deleteLearn()");
                throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.DELETE_LEARN_FAILED);
            }
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        } else {
            log.error("该学习指南不属于该企业！调用{}的{}方法出错 ", "LearnServiceImpl", "deleteLearn()");
            throw new BusinessException(ResCodeEnum.STUDY_GUIDE_NOT_ENTERPRISE, ExceptionMessageEnum.STUDY_GUIDE_NOT_ENTERPRISE);
        }

    }


    /**
     * 根据Id查询单条学习指南
     *
     * @param userInfo
     * @param id
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<LearnDTO> getLearnById(AuthPlatformUserInfo userInfo, int id) {

        Learn learn = this.selectById(id);
        LearnDTO dto = BeanUtils.copyProperties(learn, LearnDTO.class);
        if (!ObjectUtils.isEmpty(learn) && learn.getOrgId().equals(userInfo.getOrgId())) {
            // 点击量+1
            learn.setHits(learn.getHits() + 1);
            if (!this.updateById(learn)) {
                log.error("修改学习指南点击量失败！调用{}的{}方法出错 ", "LearnServiceImpl", "getLearnById()");
                throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.UPDATE_LEARN_FAILED);
            }
            // 获取姓名
            dto.setUserName(
                    ObjectUtils.isEmpty(enterprisesUsersService.getEnterpriseUserInfoById(learn.getUserId(), learn.getOrgId()).getObject()) ? null :
                            enterprisesUsersService.getEnterpriseUserInfoById(learn.getUserId(), learn.getOrgId()).getObject().getNickname());

            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

        } else {
            log.error("该学习指南不属于该企业！调用{}的{}方法出错 ", "LearnServiceImpl", "getLearnById()");
            throw new BusinessException(ResCodeEnum.STUDY_GUIDE_NOT_ENTERPRISE, ExceptionMessageEnum.STUDY_GUIDE_NOT_ENTERPRISE);
        }

    }


    /**
     * 按标题搜索文章
     *
     * @param userInfo
     * @param title
     * @return
     */
    @Override
    public ResponseResult<List<LearnDTO>> findLearnByTitle(AuthPlatformUserInfo userInfo, String title) {

        if (ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("未获得企业ID");
            throw new BusinessException(ResCodeEnum.NOT_FOUND_COMPANY_ID, ExceptionMessageEnum.ENTERPRISE_ID_NOT_EXIST);
        }

        int orgId = userInfo.getOrgId();
        // 根据企业id和请求标题模糊查询学习指南
        List<Learn> learns = this.selectList(new EntityWrapper<Learn>()
                .eq("org_id", orgId)
                .like("title", title));
        List<LearnDTO> learnDTOS = BeanUtils.assemble(LearnDTO.class, learns);
        // 获取姓名
        learnDTOS.forEach(learnDTO -> learnDTO.setUserName(
                enterprisesUsersService.getEnterpriseUserInfoById(learnDTO.getUserId(), learnDTO.getOrgId()).getObject().getNickname()));

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, learnDTOS);

    }

    /**
     * 管理员通过类型获取学习指南列表
     *
     * @param userInfo
     * @param type
     * @param page
     * @return
     */
    @Override
    public ResponseResult<List<LearnDTO>> getAllByType(AuthPlatformUserInfo userInfo, int type, Page page) {

        if (ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("未获得企业ID");
            throw new BusinessException(ResCodeEnum.NOT_FOUND_COMPANY_ID, ExceptionMessageEnum.ENTERPRISE_ID_NOT_EXIST);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        int orgId = userInfo.getOrgId();
        // 根据企业id和类型查询学习指南
        List<Learn> learns = learnMapper.selectPage(pagination, new EntityWrapper<Learn>()
                .eq("org_id", orgId)
                .eq("type", type)
                .orderBy("update_at", false));
        List<LearnDTO> learnDTOS = BeanUtils.assemble(LearnDTO.class, learns);
        // 获取姓名
        learnDTOS.forEach(learnDTO -> learnDTO.setUserName(
                enterprisesUsersService.getEnterpriseUserInfoById(learnDTO.getUserId(), learnDTO.getOrgId()).getObject().getNickname()));

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, learnDTOS, PageUtils.transToPage(pagination));

    }


    /**
     * 用户获取获取学习指南列表(没有条件的情况，只分页)
     *
     * @param userInfo
     * @param page
     * @return
     */
    @Override
    public ResponseResult<List<LearnDTO>> getAll(AuthPlatformUserInfo userInfo, Page page) {

        if (ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("未获得企业ID");
            throw new BusinessException(ResCodeEnum.NOT_FOUND_COMPANY_ID, ExceptionMessageEnum.ENTERPRISE_ID_NOT_EXIST);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        int orgId = userInfo.getOrgId();
        // 根据企业id查询学习指南以修改时间倒序
        List<Learn> learns = learnMapper.selectPage(pagination, new EntityWrapper<Learn>()
                .eq("org_id", orgId)
                .orderBy("update_at", false));
        List<LearnDTO> learnDTOS = BeanUtils.assemble(LearnDTO.class, learns);
        // 获取姓名
        learnDTOS.forEach(learnDTO -> learnDTO.setUserName(
                enterprisesUsersService.getEnterpriseUserInfoById(learnDTO.getUserId(), learnDTO.getOrgId()).getObject().getNickname()));

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, learnDTOS, PageUtils.transToPage(pagination));

    }


}
