package com.bee.platform.user.service.impl;

import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.user.dao.mapper.EnterprisesCheckAttachmentMapper;
import com.bee.platform.user.entity.EnterprisesCheckAttachment;
import com.bee.platform.user.rq.EnterprisesAttachmentRQ;
import com.bee.platform.user.service.EnterprisesCheckAttachmentService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * <p>
 * 企业附件信息表 服务实现类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-09
 */
@Service
public class EnterprisesCheckAttachmentServiceImpl extends ServiceImpl<EnterprisesCheckAttachmentMapper, EnterprisesCheckAttachment> implements EnterprisesCheckAttachmentService {

    /**
     * 保存企业附件
     *
     * @param userInfo 用户信息
     * @param rqs      附件信息
     * @return 附件id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public boolean saveEnterprisesCheckAttachment(AuthPlatformUserInfo userInfo, List<EnterprisesAttachmentRQ> rqs) {
        if(ObjectUtils.isEmpty(userInfo.getId())){
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ENTERPRISES_ATTACHMENT_SAVE_FAILED);
        }
        Integer createId =  userInfo.getId();
        String creator = userInfo.getNickname();
        Date date = new Date();
        List<EnterprisesCheckAttachment> attachmentList = BeanUtils.assemble(EnterprisesCheckAttachment.class, rqs)
                .stream()
                .map(obj -> obj.setCreateId(createId)
                        .setCreator(creator)
                        .setCreateTime(date)
                        .setModifyId(createId)
                        .setModifier(creator)
                        .setModifyTime(date)
                        .setStatus(EnumCommon.LogicStatus.NORMAL.getKey()))
                .collect(Collectors.toList());
        if(!insertBatch(attachmentList)){
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ENTERPRISES_ATTACHMENT_SAVE_FAILED);
        }
        return true;
    }


}
