package com.bee.platform.customer.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.customer.dao.mapper.AuthContactMapper;
import com.bee.platform.customer.rq.AuthContactAddRQ;
import com.bee.platform.customer.dto.AuthContactDto;
import com.bee.platform.customer.entity.AuthContact;
import com.bee.platform.customer.rq.AuthContactSelectRQ;
import com.bee.platform.customer.rq.AuthContactUpdateRQ;
import com.bee.platform.customer.service.AuthContactService;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;
import java.util.List;
import java.util.Objects;


/**
 * <p>
 * 服务实现类
 * </p>
 *
 * @author hongchuan.He
 * @since 2019-05-20
 */
@Slf4j
@Service
public class AuthContactServiceImpl extends ServiceImpl<AuthContactMapper, AuthContact> implements AuthContactService {

    @Autowired
    private AuthContactMapper authContactMapper;

    /**
     * 添加联系人
     *
     * @param rqList
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> addContact(List<AuthContactAddRQ> rqList) {
        List<AuthContact> contactList = BeanUtils.assemble(AuthContact.class, rqList);
        contactList.forEach(a -> a.setCreateTime(new Date())
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey()));
        if (!this.insertBatch(contactList)) {
            log.error("添加联系人失败 类{} 方法{}", "AuthContactServiceImpl", "add");
            return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> deleteByIds(String ids) {
        String[] idArray = ids.split(",");
        if (idArray.length <= 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.INTERFACE_ID_EMPTY);
        }
        List<Integer> idList = Lists.newArrayList();
        for (String id : idArray) {
            idList.add(Integer.valueOf(id));
        }
        if (!this.deleteBatchIds(idList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.DELETE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 更新联系人
     *
     * @param rq
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> updateContact(AuthContactUpdateRQ rq) {
        AuthContact contact = authContactMapper.selectById(rq.getId());
        if (Objects.isNull(contact)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.CONTACT_NOT_EXIST);
        }
        AuthContact newContact = BeanUtils.copyProperties(rq, AuthContact.class);
        newContact.setUpdateTime(new Date());
        if (authContactMapper.updateById(newContact) != 1) {
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 条件查询
     *
     * @param rq
     * @param page
     * @return
     */
    @Override
    public ResponseResult<List<AuthContactDto>> getList(AuthContactSelectRQ rq, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        EntityWrapper<AuthContact> wrapper = new EntityWrapper<>();
        if (!StringUtils.isBlank(rq.getName())) {
            wrapper.in("name", rq.getName());
        }
        if (Objects.nonNull(rq.getContactNo())) {
            wrapper.in("ContactNo", rq.getContactNo());
        }
        if (Objects.nonNull(rq.getPhone())) {
            wrapper.in("Phone", rq.getPhone());
        }
        if (Objects.nonNull(rq.getStarLevel())) {
            wrapper.in("StarLevel", rq.getStarLevel().toString());
        }
        if (Objects.nonNull(rq.getSecondType())) {
            wrapper.in("SecondType", rq.getSecondType());
        }

        List<AuthContact> authContact = authContactMapper.selectPage(pagination, wrapper);
        List<AuthContactDto> result = BeanUtils.assemble(AuthContactDto.class, authContact);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result, PageUtils.transToPage(pagination));
    }

}
